module Rules exposing (markedHints, solved)
-- Rules of Picross, game logic.

import Grid as G
import Core exposing (..)

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

hints : G.Grid Bool -> (List (List Int), List (List Int))
hints grid =
    let
        transpose = case G.transpose grid of
            Nothing -> Debug.crash "Couldn't transpose grid"
            Just t -> t
        fmt g = List.map flattenBools <| G.toLists g
        cols = fmt grid
        rows = fmt transpose
    in
        (cols, rows)

solved : G.Grid Bool -> G.Grid Mark -> Bool
-- Has this puzzle been solved (i.e. has the user marked all blocks)?
solved puzzle progress =
    puzzle == G.map (\m -> case m of
        Black -> True
        White -> False
        Dot -> False)
    progress

markedHints : G.Grid Bool -> G.Grid Mark -> (List (List Hint), List (List Hint))
-- TODO: implement this.
markedHints puzzle progress =
    let
        (cols, rows) = hints puzzle
        addHintType = List.map (\i -> (i, Todo))
    in
        (List.map addHintType cols, List.map addHintType rows)