module Grid exposing (..)

import Array as A
import Maybe exposing (andThen)

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

get : Int -> Int -> Grid a -> Maybe a
get i j grid = (A.get j grid) |> andThen (A.get i)

set : Int -> Int -> a -> Grid a -> Grid a
set j i val grid =
    let
        row = A.get i grid
    in
        case row of
            Just r -> (A.set i (A.set j val r) grid)
            Nothing -> grid

transpose : Grid a -> Maybe (Grid a)
transpose grid =
    let
        r = List.range 0 (-1 + A.length grid)
    in
        Maybe.map A.fromList (unmaybeList <| List.map (\i -> row i grid) r)

row : Int -> Grid a -> Maybe (A.Array a)
row i grid = unmaybeArray <| A.map (A.get i) grid

unmaybeList : List (Maybe a) -> Maybe (List a)
unmaybeList elems =
    case elems of
        [] ->
            Just []
        (e::es) ->
            case (e, unmaybeList es) of
                (_, Nothing) -> Nothing
                (Nothing, _) -> Nothing
                (Just x, Just xs) -> Just (x::xs)

unmaybeArray : A.Array (Maybe a) -> Maybe (A.Array a)
unmaybeArray arr =
    Maybe.map A.fromList (unmaybeList <| (A.toList arr))