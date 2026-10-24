module Kernel exposing
    ( Closing(..)
    , Dissipation
    , Pool
    , Release(..)
    , WhenSpent(..)
    , Witness(..)
    , dissipate
    , held
    , pool
    , preflight
    , refusal
    , releaseFromString
    , released
    , witness
    )

{-| How a connection whose request body was never read is closed, and the
pool of connections that may linger while they are.

Closing at once loses answers: the client is still sending, the kernel
answers its next bytes with a reset, and a client whose send fails that way
can give up without reading the response already waiting for it (RFC 9112
section 9.6). So such a connection is dissipated: the bridge sends FIN after
the response, then reads and drops the client's bytes until the client hangs
up, the deadline passes, or the budget is spent.

The pool is Elm's own exact count. A slot is held from the moment Elm
decides to dissipate until the bridge reports the connection's fd closed, so
the count is never low, and the bridge never compares anything with a cap.

-}

import Http.Request as Request exposing (ConnId)
import Http.Response as Response exposing (Response)
import Http.Route as Route exposing (Route)
import Io exposing (HostError)
import Layout exposing (Final, Layout)
import Set exposing (Set)



-- DISSIPATION


{-| `budgetBytes` stops a fast sender; `deadlineMs` stops a slow one, which
never comes near the budget.
-}
type alias Dissipation =
    { deadlineMs : Int
    , budgetBytes : Int
    , whenSpent : WhenSpent
    }


{-| What the bridge does once the budget is spent. Measured on loopback
against clients still sending when the refusal arrived:

`Cut` closes at once and frees the fd and the slot. A loopback sender
spends the budget in microseconds, so the close comes barely later than an
immediate one: curl got its answer 399 times in 400, Node's client 168 in 250.

`Hold` stops reading and closes at the deadline. The client's sends block
instead of failing, so it has time to read: curl 400 in 400, Node's client
239 in 250. But a socket that is not read cannot see its peer hang up, so
every fast sender holds its slot for the whole deadline.

-}
type WhenSpent
    = Cut
    | Hold


type Closing
    = Dissipating Dissipation
    | Severed


{-| A slot if the pool has one, and the policy for its budget: hold while
at least half the pool would still be free, cut under pressure, so an idle
server delivers its refusals and a busy one keeps its fds.
-}
dissipate : { deadlineMs : Int, budgetBytes : Int } -> ConnId -> Pool -> ( Closing, Pool )
dissipate bounds conn (Pool p) =
    let
        taken =
            Set.size p.held
    in
    if taken < p.cap then
        ( Dissipating
            { deadlineMs = bounds.deadlineMs
            , budgetBytes = bounds.budgetBytes
            , whenSpent =
                if 2 * (taken + 1) <= p.cap then
                    Hold

                else
                    Cut
            }
        , Pool { p | held = Set.insert (Request.connIdToString conn) p.held }
        )

    else
        ( Severed, Pool p )



-- THE POOL


type Pool
    = Pool { cap : Int, held : Set String }


pool : Int -> Pool
pool cap =
    Pool { cap = cap, held = Set.empty }


held : Pool -> Int
held (Pool p) =
    Set.size p.held


{-| The slot is free again. A connection that holds no slot is `Nothing`,
never silently ignored.
-}
released : ConnId -> Pool -> Maybe Pool
released conn (Pool p) =
    let
        key =
            Request.connIdToString conn
    in
    if Set.member key p.held then
        Just (Pool { p | held = Set.remove key p.held })

    else
        Nothing


{-| How a dissipated connection ended. `BudgetSpent` covers both `Cut` and
a `Hold` that ran to the deadline.
-}
type Release
    = HungUp
    | DeadlinePassed
    | BudgetSpent


releaseFromString : String -> Maybe Release
releaseFromString how =
    case how of
        "hungUp" ->
            Just HungUp

        "deadline" ->
            Just DeadlinePassed

        "budget" ->
            Just BudgetSpent

        _ ->
            Nothing



-- PRE-FLIGHT


{-| What probing a final name proved.

Final names are made only by `link(2)`, which never replaces one, and the
server never removes one. So `Present` is stationary for the one question
it answers, "may this upload commit" (L-spec Theorem 15): once seen, it
stays true. `NotSeen` is not: another upload can commit between the probe
and our link. A witness can therefore justify a refusal but never grant a
commit; after `NotSeen`, the link still decides.

-}
type Witness
    = Present
    | NotSeen


{-| The final name a route must probe before it lets a body in: uploads
and ingests are create-only, so if the name exists the answer is 409
whatever the body holds.
-}
preflight : Layout -> Route -> Maybe Final
preflight layout route =
    case route of
        Route.Upload id ->
            Just (Layout.blob layout id)

        Route.Ingest id ->
            Just (Layout.ingested layout id)

        Route.Health ->
            Nothing

        Route.Item _ ->
            Nothing

        Route.Echo ->
            Nothing

        Route.Delayed _ ->
            Nothing

        Route.Stats ->
            Nothing

        Route.Blob _ ->
            Nothing


{-| A probe's outcome as a witness. ENOENT is the one failure that means
"not there"; any other is a host error, not evidence of absence.
-}
witness : Result HostError a -> Result HostError Witness
witness outcome =
    case outcome of
        Ok _ ->
            Ok Present

        Err error ->
            case Io.classify error of
                Io.NoEntry ->
                    Ok NotSeen

                _ ->
                    Err error


refusal : Witness -> Maybe Response
refusal seen =
    case seen of
        Present ->
            Just (Response.problem Response.Conflict "this id has already been committed" [])

        NotSeen ->
            Nothing
