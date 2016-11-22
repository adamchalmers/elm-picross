import Html exposing (Html, Attribute, beginnerProgram, text, div, input, span)
import Maybe

import Core exposing (..)
import Gui exposing (view)
import Grid as G

main : Program Never Model Msg
main =
  beginnerProgram { model = easy, view = Gui.view, update = update }

easy : Model
easy =
    Model easyPic (G.initialize (G.length easyPic) (G.height easyPic) False) "console"

easyPic : G.Grid Bool
easyPic =
    G.fromLists
        [ [False, False, False, True, True, True]
        , [False, False, False, False, False, True]
        , [False, False, False, False, True, True]
        , [False, False, False, False, True, True]
        , [True, False, False, False, True, True]
        , [True, False, False, False, True, True]
        ]

update : Msg -> Model -> Model
update msg model =
    case msg of
        Noop ->
            model
        Click x y ->
            let
                newProgress = G.set x y (not <| Maybe.withDefault True <| G.get x y model.progress) model.progress
            in
                { model
                | progress = newProgress
                , console =
                    if newProgress == model.puzzle
                    then "Win"
                    else model.console
                }
