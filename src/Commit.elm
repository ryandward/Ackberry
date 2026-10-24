module Commit exposing
    ( Fill(..)
    , Payload
    , Spec
    , ingest
    , judge
    , parsePayload
    , saga
    , upload
    )

{-| Create-only commit of bytes to a final name, as one transaction.

    open(part, O_CREAT|O_EXCL)  the lease: a part named after this transaction
                                registers: Always close, OnFailure unlink part
    write | pipe the body       a buffered payload, or the socket straight to disk
    fsync(fd)                   the bytes are durable before any name points at them
    link(part, final)           COMMIT: create-only, EEXIST if final exists
    unlink(part)                roll forward
    fsync(final's dir)          roll forward: the new name survives a crash
    close(fd)                   the Always cleanup, last

Before the commit, any failure or abort unwinds to: unlink part, close.
After it, nothing is undone. The fd is closed after the link rather than
before so that one `Always` cleanup releases it on every path; the data was
already fsync'd, so the order changes nothing on disk.

The part is anonymous (see `Layout`): a transaction killed by SIGKILL leaves
a corpse that blocks nothing, because the next transaction for the same id
writes its own part. Uploads to one id do not exclude each other while they
stream; they race at the commit, and `link(2)` lets exactly one win.

The commit is link(2), not rename(2). With anonymous parts nothing else
stops two uploads to one id from both reaching the commit, and rename would
let the second silently replace the first after both clients were told 201:
a lost write. renameat2(RENAME\_NOREPLACE) would be one call, but Node does
not expose it. link-then-unlink gives the same guarantee on the final name:
it appears atomically, holding complete fsync'd bytes, exactly once.

-}

import Domain exposing (ItemId)
import Http.Request as Request exposing (JsonText, RequestId)
import Http.Response as Response exposing (Response)
import Io exposing (Handle, HostError)
import Json.Decode as Decode
import Json.Encode as Encode
import Layout exposing (Final, Layout, Part)
import Plan exposing (Plan)
import Tx
import Verdict exposing (Fault(..), Verdict)


{-| A request body proven to be a JSON object, kept byte-for-byte so the
digest a client computes over what it sent matches the one returned.
-}
type Payload
    = Payload JsonText


parsePayload : JsonText -> Result String Payload
parsePayload text =
    case Decode.decodeString (Decode.keyValuePairs Decode.value) (Request.jsonTextToString text) of
        Ok _ ->
            Ok (Payload text)

        Err _ ->
            Err "the ingest payload must be a JSON object"


type Fill
    = Buffered Payload
    | Streamed { maxBytes : Int, sendContinue : Bool }


type alias Spec =
    { label : String
    , id : ItemId
    , part : Part
    , final : Final
    , lease : Handle
    , fill : Fill
    }


ingest : Layout -> RequestId -> ItemId -> Payload -> Spec
ingest layout request id payload =
    { label = "itemId"
    , id = id
    , part = Layout.part layout request
    , final = Layout.ingested layout id
    , lease = Io.handle (Request.idToString request) "lease"
    , fill = Buffered payload
    }


upload : Layout -> RequestId -> ItemId -> { maxBytes : Int, sendContinue : Bool } -> Spec
upload layout request id body =
    { label = "blobId"
    , id = id
    , part = Layout.part layout request
    , final = Layout.blob layout id
    , lease = Io.handle (Request.idToString request) "lease"
    , fill = Streamed body
    }


{-| The whole transaction. Its shape is fixed by `Plan`'s types: the lease
brings its cleanups, the bytes go to the leased fd, and only fsync'd bytes
reach `commit`.
-}
saga : Spec -> Plan Plan.Fresh -> Plan.Program
saga spec plan =
    plan
        |> Plan.lease { handle = spec.lease, part = spec.part }
        |> fill spec.fill
        |> Plan.sync
        |> Plan.commit spec.final
        |> Plan.settle


fill : Fill -> Plan Plan.Leased -> Plan Plan.Filled
fill how =
    case how of
        Buffered (Payload text) ->
            Plan.write (Request.jsonTextToString text)

        Streamed body ->
            Plan.pipe body


{-| Read the settled report against the plan's step positions:
0 open, 1 fill, 2 fsync, 3 link, 4 unlink part, 5 fsync the final's dir.
-}
judge : Spec -> Tx.Report -> Verdict
judge plan report =
    if report.committed then
        case at 1 report.steps |> Maybe.andThen Result.toMaybe |> Maybe.map (Decode.decodeValue receiptDecoder) of
            Just (Ok receipt) ->
                let
                    orphan =
                        case at 4 report.steps of
                            Just (Err error) ->
                                Verdict.orphanUnlessGone (Layout.partPath plan.part) error

                            _ ->
                                []
                in
                case at 5 report.steps of
                    Just (Err error) ->
                        ( Just
                            (Verdict.hostProblem Response.InternalServerError
                                "the bytes are stored but the directory sync failed, so their durability is unconfirmed"
                                error
                            )
                        , orphan ++ [ Unclassified "syncing the data directory after commit" error ]
                        )

                    _ ->
                        ( Just (created plan receipt), orphan )

            _ ->
                ( Just (Response.problem Response.InternalServerError "the I/O bridge broke its protocol" [])
                , [ Anomaly "a commit settled without a receipt for its bytes" ]
                )

    else
        case lastFailure report.steps of
            Just ( 0, error ) ->
                acquireFailure error

            Just ( 1, error ) ->
                fillFailure error

            Just ( 2, error ) ->
                Verdict.diskFailure "flushing the bytes" error

            Just ( 3, error ) ->
                commitFailure error

            Just ( _, error ) ->
                Verdict.fatal "a step after the commit point that never ran" error

            Nothing ->
                -- Aborted between steps: whoever aborted already answered.
                Verdict.quiet


at : Int -> List a -> Maybe a
at n list =
    List.head (List.drop n list)


lastFailure : List (Result HostError Decode.Value) -> Maybe ( Int, HostError )
lastFailure outcomes =
    case List.reverse outcomes of
        (Err error) :: _ ->
            Just ( List.length outcomes - 1, error )

        _ ->
            Nothing


{-| Nothing about the id can make this fail: the part's name belongs to
this transaction alone. So EEXIST is not a conflict but a broken invariant,
two transactions given one name.
-}
acquireFailure : HostError -> Verdict
acquireFailure error =
    case Io.classify error of
        Io.Exists ->
            ( Just (Verdict.hostProblem Response.InternalServerError "the transaction's part name was already taken" error)
            , [ Anomaly "a part named after a fresh transaction already existed: transaction names were reused" ]
            )

        Io.NoEntry ->
            ( Just (Verdict.hostProblem Response.InternalServerError "the parts directory does not exist" error), [] )

        Io.Denied ->
            Verdict.diskFailure "acquiring the lease" error

        Io.NoSpace ->
            Verdict.diskFailure "acquiring the lease" error

        Io.Exhausted ->
            Verdict.diskFailure "acquiring the lease" error

        Io.DeviceError ->
            Verdict.diskFailure "acquiring the lease" error

        Io.Cancelled ->
            Verdict.quiet

        Io.Refused ->
            Verdict.fatal "acquiring the lease" error

        Io.Reset ->
            Verdict.fatal "acquiring the lease" error

        Io.Unreachable ->
            Verdict.fatal "acquiring the lease" error

        Io.TooLarge ->
            Verdict.fatal "acquiring the lease" error

        Io.Unrecognized ->
            Verdict.fatal "acquiring the lease" error


fillFailure : HostError -> Verdict
fillFailure error =
    case Io.classify error of
        Io.TooLarge ->
            ( Just (Verdict.hostProblem Response.PayloadTooLarge "the body exceeded the size limit while streaming" error), [] )

        Io.Reset ->
            -- The client hung up mid-body. Nobody will read this answer.
            ( Just (Verdict.hostProblem Response.BadRequest "the request body was cut off" error), [] )

        Io.Cancelled ->
            Verdict.quiet

        Io.NoSpace ->
            Verdict.diskFailure "writing the bytes" error

        Io.Exhausted ->
            Verdict.diskFailure "writing the bytes" error

        Io.Denied ->
            Verdict.diskFailure "writing the bytes" error

        Io.DeviceError ->
            Verdict.diskFailure "writing the bytes" error

        Io.NoEntry ->
            Verdict.fatal "writing the bytes" error

        Io.Exists ->
            Verdict.fatal "writing the bytes" error

        Io.Refused ->
            Verdict.fatal "writing the bytes" error

        Io.Unreachable ->
            Verdict.fatal "writing the bytes" error

        Io.Unrecognized ->
            Verdict.fatal "writing the bytes" error


commitFailure : HostError -> Verdict
commitFailure error =
    case Io.classify error of
        Io.Exists ->
            ( Just (Verdict.hostProblem Response.Conflict "this id has already been committed" error), [] )

        Io.NoEntry ->
            -- Some other process removed the part under a live lease: a
            -- second server sharing this directory with a shorter
            -- staleAfterMs, or a person.
            Verdict.fatal "committing: the part vanished under its lease" error

        Io.Cancelled ->
            Verdict.quiet

        Io.Denied ->
            Verdict.diskFailure "committing" error

        Io.NoSpace ->
            Verdict.diskFailure "committing" error

        Io.Exhausted ->
            Verdict.diskFailure "committing" error

        Io.DeviceError ->
            Verdict.diskFailure "committing" error

        Io.Refused ->
            Verdict.fatal "committing" error

        Io.Reset ->
            Verdict.fatal "committing" error

        Io.Unreachable ->
            Verdict.fatal "committing" error

        Io.TooLarge ->
            Verdict.fatal "committing" error

        Io.Unrecognized ->
            Verdict.fatal "committing" error


type alias Receipt =
    { bytes : Int
    , sha256 : String
    }


receiptDecoder : Decode.Decoder Receipt
receiptDecoder =
    Decode.map2 Receipt
        (Decode.field "bytes" Decode.int)
        (Decode.field "sha256" Decode.string)


created : Spec -> Receipt -> Response
created plan receipt =
    Response.created
        (Encode.object
            [ ( plan.label, Encode.int (Domain.itemIdToInt plan.id) )
            , ( "bytes", Encode.int receipt.bytes )
            , ( "sha256", Encode.string receipt.sha256 )
            ]
        )
