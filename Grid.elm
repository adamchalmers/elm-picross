module Grid exposing (..)

import Array as A

type alias Grid a = A.Array (A.Array a)

map : (a -> b) -> Grid a -> Grid b
map f grid =
    A.map (A.map f) grid

indexedMap : (Int -> Int -> a -> b) -> Grid a -> Grid b
indexedMap f grid =
    let
        rowFn : Int -> (A.Array a) -> A.Array b
        rowFn i row = A.indexedMap (\j elem -> f i j elem) row
    in
        A.indexedMap rowFn grid

toLists : Grid a -> List (List a)
toLists g = A.toList <| A.map (A.toList) g