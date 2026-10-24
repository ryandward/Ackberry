module Plan exposing
    ( Committed
    , Durable
    , Filled
    , Fresh
    , Leased
    , Plan
    , Program
    , Reading
    , Suspended
    , commit
    , continuation
    , discard
    , end
    , fresh
    , httpGet
    , lease
    , openRead
    , pipe
    , prepare
    , readBody
    , release
    , resume
    , settle
    , stat
    , steps
    , stream
    , survey
    , suspend
    , sync
    , write
    )

{-| Typed construction of transactions.

`Tx` defines what a transaction means. This module decides which ones can
be written. A plan holds its capability as a value whose type is its state,
and every step's signature names the state it needs and the state it leaves:

    fresh                        : Plan Fresh
    lease { handle, part }       : Plan Fresh -> Plan Leased      close and unlink registered here
    write text, pipe body        : Plan Leased -> Plan Filled
    sync                         : Plan Filled -> Plan Durable
    commit final                 : Plan Durable -> Plan Committed
    settle                       : Plan Committed -> Program      unlink the part, fsync the final's dir

    openRead { handle, path }    : Plan Fresh -> Plan Reading     close registered here
    stat, stream head            : Plan Reading -> Plan Reading
    release                      : Plan Reading -> Program

    prepare, survey, discard     : Plan Fresh -> Plan Fresh       the janitor's steps

So the compiler refuses: a lease without its cleanups (`lease` is the only
way to reach `Leased`), a lease on anything but an anonymous part (a lease
takes a `Layout.Part`, and no part can be named after an item id), bytes
written anywhere but the leased fd, a commit of bytes not yet fsync'd, a
second commit, any step after a capability is given up, and a janitor that
deletes anything but parts. Every state's constructor is private, and
`Program`, the only thing `Server` can send, is produced only here.

What it cannot do, in Elm:

  - No linear types: a `Plan Leased` value can be copied, and two programs
    built from it. Within one program the steps are a single chain.
  - No existential types or GADTs: a continuation that depends on a result
    is not part of a program, and could not be serialized if it were. A
    program `suspend`s instead; Elm reads the report and `resume`s with the
    capability the pause handed back. That is the only bind, and it always
    crosses the port.

-}

import Io exposing (Handle)
import Layout exposing (Final, Layout, Part)
import Tx


type Plan state
    = Plan (List Tx.Step) state


{-| A finished plan and how it ends: `Pause` if it was suspended, `Finish`
otherwise. Built only by `end`, `settle`, `release` and `suspend`.
-}
type Program
    = Program (List Tx.Step) Tx.Continuation


type alias Lease =
    { handle : Handle
    , part : Part
    }


{-| Holds nothing.
-}
type Fresh
    = Fresh


{-| Holds a part this transaction created and its open fd; the close and
the unlink that undo them are already registered.
-}
type Leased
    = Leased Lease


{-| The bytes are in the part.
-}
type Filled
    = Filled Lease


{-| The bytes are on disk.
-}
type Durable
    = Durable Lease


{-| The final name points at the durable bytes. Nothing will be undone.
-}
type Committed
    = Committed Lease Final


{-| Holds a file open for reading; its close is already registered.
-}
type Reading
    = Reading Handle


{-| A capability carried across a pause.
-}
type Suspended state
    = Suspended state


fresh : Plan Fresh
fresh =
    Plan [] Fresh


append : Tx.Step -> next -> Plan state -> Plan next
append step next (Plan done _) =
    Plan (done ++ [ step ]) next


readBody : { maxBytes : Int, sendContinue : Bool } -> Plan Fresh -> Plan Fresh
readBody body plan =
    append (Tx.Do (Io.ReadBody body) []) Fresh plan


httpGet : { url : String, maxBytes : Int } -> Plan Fresh -> Plan Fresh
httpGet get plan =
    append (Tx.Do (Io.HttpGet get) []) Fresh plan


end : Plan Fresh -> Program
end (Plan done Fresh) =
    Program done Tx.Finish



-- WRITING


{-| `O_EXCL` on a name no other transaction can hold: it fails only if
the host is broken or ids were reused, never because of another upload.
-}
lease : { handle : Handle, part : Part } -> Plan Fresh -> Plan Leased
lease l plan =
    append
        (Tx.Do (Io.Open { handle = l.handle, mode = Io.CreateExclusive, path = Layout.partPath l.part })
            [ Tx.Always (Io.Close l.handle), Tx.OnFailure (Io.Unlink (Layout.partPath l.part)) ]
        )
        (Leased l)
        plan


write : String -> Plan Leased -> Plan Filled
write text ((Plan _ (Leased l)) as plan) =
    append (Tx.Do (Io.Write l.handle text) []) (Filled l) plan


pipe : { maxBytes : Int, sendContinue : Bool } -> Plan Leased -> Plan Filled
pipe body ((Plan _ (Leased l)) as plan) =
    append (Tx.Do (Io.PipeBody { handle = l.handle, maxBytes = body.maxBytes, sendContinue = body.sendContinue }) []) (Filled l) plan


sync : Plan Filled -> Plan Durable
sync ((Plan _ (Filled l)) as plan) =
    append (Tx.Do (Io.Sync l.handle) []) (Durable l) plan


{-| The pivot. Only durable bytes can be given their final name, and
`link(2)` gives it only if nobody has: the final name appears atomically,
complete, or not at all.
-}
commit : Final -> Plan Durable -> Plan Committed
commit final ((Plan _ (Durable l)) as plan) =
    append (Tx.Commit (Io.Link { from = Layout.partPath l.part, to = Layout.finalPath final })) (Committed l final) plan


{-| Roll forward: drop the part's name and make the final name durable.
The registered close runs last. If a crash loses the unlink, the part comes
back as a second name for committed bytes, and the janitor removes it.
-}
settle : Plan Committed -> Program
settle ((Plan _ (Committed l final)) as plan) =
    let
        (Plan done _) =
            plan
                |> append (Tx.Do (Io.Unlink (Layout.partPath l.part)) []) ()
                |> append (Tx.Do (Io.SyncDir (Layout.finalDir final)) []) ()
    in
    Program done Tx.Finish



-- READING


openRead : { handle : Handle, path : String } -> Plan Fresh -> Plan Reading
openRead r plan =
    append (Tx.Do (Io.Open { handle = r.handle, mode = Io.ReadOnly, path = r.path }) [ Tx.Always (Io.Close r.handle) ]) (Reading r.handle) plan


stat : Plan Reading -> Plan Reading
stat ((Plan _ (Reading h)) as plan) =
    append (Tx.Do (Io.Stat h) []) (Reading h) plan


stream : { length : Int, status : Int, headers : List String } -> Plan Reading -> Plan Reading
stream head ((Plan _ (Reading h)) as plan) =
    append (Tx.Do (Io.Stream { handle = h, length = head.length, status = head.status, headers = head.headers }) []) (Reading h) plan


{-| Give the file back: the registered close runs when the program ends.
-}
release : Plan Reading -> Program
release (Plan done _) =
    Program done Tx.Finish



-- THE JANITOR


{-| Create the parts and blob directories if they are missing, durably.
-}
prepare : Layout -> Plan Fresh -> Plan Fresh
prepare layout plan =
    plan
        |> append (Tx.Do (Io.EnsureDir (Layout.parts layout)) []) Fresh
        |> append (Tx.Do (Io.EnsureDir (Layout.blobs layout)) []) Fresh


{-| List the parts directory, with the host's clock at the moment of listing.
-}
survey : Layout -> Plan Fresh -> Plan Fresh
survey layout plan =
    append (Tx.Do (Io.ListDir (Layout.parts layout)) []) Fresh plan


{-| Remove parts, each independently. Only parts: the janitor cannot be
told to delete a final name.
-}
discard : List Part -> Plan Fresh -> Plan Fresh
discard doomed plan =
    append (Tx.Do (Io.UnlinkEach (List.map Layout.partPath doomed)) []) Fresh plan



-- PAUSES


suspend : Plan state -> ( Program, Suspended state )
suspend (Plan done state) =
    ( Program done Tx.Pause, Suspended state )


resume : Suspended state -> Plan state
resume (Suspended state) =
    Plan [] state



-- READING A PROGRAM


steps : Program -> List Tx.Step
steps (Program done _) =
    done


continuation : Program -> Tx.Continuation
continuation (Program _ how) =
    how
