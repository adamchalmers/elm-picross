module Rules exposing (..)
-- Rules of Picross, game logic.

import Grid as G
import Maybe as M

flattenBools : List Bool -> List Int
-- Convenience wrapper around countBools
flattenBools bools =
    case countBools [] False bools of
        [] -> [0]
        other -> other

countBools : List Int -> Bool -> List Bool -> List Int
-- The length of each run of Trues, e.g. [T,F,F,T,T] -> [1,2] and [F, F] -> []
countBools accum prev bools =
    case (prev, bools) of
        (_, []) ->
            List.reverse accum
        (_, (False::bs)) ->
            countBools accum False bs
        (True, (True::bs)) ->
            case accum of
                (x::xs) -> countBools (x+1::xs) True bs
                [] -> countBools [1] True bs
        (False, (True::bs)) ->
            countBools (1::accum) True bs

gridHeaders : G.Grid Bool -> (List String, List String)
-- Returns column/row headers for a Picross grid.
gridHeaders grid =
    let
        transpose = case G.transpose grid of
            Nothing -> Debug.crash "Couldn't transpose grid"
            Just t -> t
        fmt g = G.toLists g |> List.map (toString << flattenBools)
        cols = fmt grid
        rows = fmt transpose
    in
        (cols, rows)