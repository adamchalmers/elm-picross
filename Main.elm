import Html exposing (Html, Attribute, beginnerProgram, text, div, input, span)
import Array as A
import Array
import Maybe

import Core exposing (..)
import Gui exposing (view)
import Grid

main : Program Never Model Msg
main =
  beginnerProgram { model = easy, view = Gui.view, update = update }

easy : Model
easy =
    let
        console = "console"
        -- console = toString <| Rules.flattenBools [False]
    in
        Model easyPic (A.initialize 5 (always (A.initialize 5 (always False)))) console

easyPic : Grid.Grid Bool
easyPic =
    A.fromList
        [ A.fromList [False, False, False, False, False]
        , A.fromList [False, False, False, False, False]
        , A.fromList [False, False, False, False, True]
        , A.fromList [False, False, False, False, True]
        , A.fromList [False, False, False, False, True]
        ]

update : Msg -> Model -> Model
update msg model =
    case msg of
        Noop ->
            model
        Click x y ->
            let
                newProgress = Grid.set x y (not <| Maybe.withDefault True <| Grid.get x y model.progress) model.progress
            in
                { model
                | console = "clicked square [" ++ toString x ++ ", " ++ toString y ++ "]"
                , progress = newProgress
                , console =
                    if newProgress == model.puzzle
                    then "Win"
                    else model.console
                }
