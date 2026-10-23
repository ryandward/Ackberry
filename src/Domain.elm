module Domain exposing
    ( Item
    , ItemId
    , encodeItem
    , itemDecoder
    , itemIdFromInt
    , itemIdToInt
    , lookupItem
    , maxItemId
    )

{-| Mock application domain: a fixed item catalog.

`ItemId` is opaque so that every value of it is already known to be in range.
The range stops at 2^31 - 1 because Elm's `//` and bitwise operators truncate
through 32-bit integers; a larger id would survive parsing and then corrupt
the first time someone did arithmetic on it.

-}

import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


type ItemId
    = ItemId Int


maxItemId : Int
maxItemId =
    2147483647


itemIdFromInt : Int -> Maybe ItemId
itemIdFromInt n =
    if n >= 1 && n <= maxItemId then
        Just (ItemId n)

    else
        Nothing


itemIdToInt : ItemId -> Int
itemIdToInt (ItemId n) =
    n


type alias Item =
    { id : ItemId
    , name : String
    , priceCents : Int
    }


catalog : Dict Int Item
catalog =
    [ Item (ItemId 1) "Anvil" 18900
    , Item (ItemId 7) "Bellows" 4250
    , Item (ItemId 42) "Towel" 1200
    , Item (ItemId 123) "Tongs" 2799
    ]
        |> List.map (\item -> ( itemIdToInt item.id, item ))
        |> Dict.fromList


lookupItem : ItemId -> Maybe Item
lookupItem id =
    Dict.get (itemIdToInt id) catalog


encodeItem : Item -> Encode.Value
encodeItem item =
    Encode.object
        [ ( "id", Encode.int (itemIdToInt item.id) )
        , ( "name", Encode.string item.name )
        , ( "priceCents", Encode.int item.priceCents )
        ]


{-| An item as another service describes it. The id goes through the same
range check as one parsed from a URL.
-}
itemDecoder : Decoder Item
itemDecoder =
    Decode.map3 Item
        (Decode.field "id" Decode.int
            |> Decode.andThen
                (\n ->
                    case itemIdFromInt n of
                        Just id ->
                            Decode.succeed id

                        Nothing ->
                            Decode.fail ("item id " ++ String.fromInt n ++ " is out of range")
                )
        )
        (Decode.field "name" Decode.string)
        (Decode.field "priceCents" Decode.int)
