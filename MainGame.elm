import Html exposing (Html, Attribute, beginnerProgram, text, div, input, span)
import Maybe
import Debug

import Core exposing (..)
import Gui exposing (view)
import Grid as G
import Rules

main : Program Never Model Msg
main =
  beginnerProgram { model = easy, view = Gui.view, update = update }

easy : Model
easy =
    Model easyPic (G.initialize (G.length easyPic) (G.height easyPic) White) ""

easyPic : G.Grid Bool
easyPic =
    G.fromLists
        [ [False, False, True, True]
        , [False, True, False, False]
        , [True, False, True, True]
        , [False, True, True, True]
        ]

update : Msg -> Model -> Model
update msg model =
    case msg of
        Noop ->
            model
        Click x y ->
            let
                mark = case G.get x y model.progress of
                    Just m -> m
                    Nothing -> Debug.crash <| "You've clicked an invalid square " ++ (toString x) ++ " " ++ (toString y)
                newProgress = G.set x y (changeMark mark) model.progress
            in
                { model
                | progress = newProgress
                , console =
                    if (Rules.solved model.puzzle newProgress)
                    then "You win!"
                    else ""
                }

changeMark : Mark -> Mark
changeMark m = case m of
    White -> Black
    Black -> Dot
    Dot -> White
