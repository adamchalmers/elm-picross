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
    Model easyPic (A.initialize 5 (always (A.initialize 5 (always False)))) "console"

easyPic : Grid.Grid Bool
easyPic =
    A.fromList
        [ A.fromList [False, False, False, False, False]
        , A.fromList [False, False, False, False, False]
        , A.fromList [False, False, False, False, True]
        , A.fromList [False, False, False, False, True]
        , A.fromList [False, False, False, False, True]
        ]



-- UPDATE

update : Msg -> Model -> Model
update msg model =
    case msg of
        Noop ->
            model
        Click x y ->
            { model
            | console = "clicked square [" ++ toString x ++ ", " ++ toString y ++ "]"
            , progress = (Grid.set x y (not <| Maybe.withDefault True <| Grid.get x y model.progress) model.progress)
            }