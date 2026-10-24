module Layout exposing
    ( Final
    , Layout
    , Part
    , blob
    , blobs
    , finalDir
    , finalPath
    , fromDataDir
    , ingested
    , part
    , partFromName
    , partPath
    , parts
    )

{-| Every path the server touches, and the only way to name one.

    <data>/.tmp/<transaction>.part    in-flight bytes, one name per transaction
    <data>/blobs/<id>                 committed blobs
    <data>/file_<id>.final            committed ingest payloads

A part is named after the transaction that writes it, never after the
resource it will become, and there is no way to build a `Part` from an item
id. So a part can never stand for "this id is taken": it is either being
written by a live transaction or it is a corpse, and either way a new
transaction for the same id writes its own part next to it. The one arbiter
of whether an id exists is its final name, which only `link(2)` creates, and
which fails with EEXIST if it is already there.

The parts directory sits inside the data directory so that every part is on
the same filesystem as every final name, which `link(2)` requires.

-}

import Domain exposing (ItemId)
import Http.Request as Request exposing (RequestId)


type Layout
    = Layout String


fromDataDir : String -> Layout
fromDataDir =
    Layout


parts : Layout -> String
parts (Layout data) =
    data ++ "/.tmp"


blobs : Layout -> String
blobs (Layout data) =
    data ++ "/blobs"



-- PARTS


type Part
    = Part { dir : String, stem : String }


{-| The part a request's transaction writes. Request ids are minted by the
bridge with `randomUUID`, so no two transactions, in this process or any
earlier one, name the same part.
-}
part : Layout -> RequestId -> Part
part layout request =
    Part { dir = parts layout, stem = Request.idToString request }


partPath : Part -> String
partPath (Part p) =
    p.dir ++ "/" ++ p.stem ++ ".part"


{-| A name read from the parts directory, if `part` could have produced it.
The inverse of `part` for every id that can be a file name, which every
UUID is. Anything else in the directory is not ours and is left alone.
-}
partFromName : Layout -> String -> Maybe Part
partFromName layout name =
    let
        stem =
            String.dropRight 5 name
    in
    if String.endsWith ".part" name && not (String.isEmpty stem) && not (String.contains "/" stem) then
        Just (Part { dir = parts layout, stem = stem })

    else
        Nothing



-- FINAL NAMES


type Final
    = Final { dir : String, name : String }


blob : Layout -> ItemId -> Final
blob layout id =
    Final { dir = blobs layout, name = String.fromInt (Domain.itemIdToInt id) }


ingested : Layout -> ItemId -> Final
ingested (Layout data) id =
    Final { dir = data, name = "file_" ++ String.fromInt (Domain.itemIdToInt id) ++ ".final" }


finalPath : Final -> String
finalPath (Final f) =
    f.dir ++ "/" ++ f.name


{-| The directory whose entry for the final name must be fsync'd for the
commit to survive a crash.
-}
finalDir : Final -> String
finalDir (Final f) =
    f.dir
