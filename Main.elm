import Html exposing (Html, Attribute, beginnerProgram, text, div, input, span)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Array as A
import Array
import List

import Core exposing (..)
import Grid exposing (..)

main : Program Never Model Msg
main =
  beginnerProgram { model = easy, view = view, update = update }

easy : Model
easy =
    Model easyPic (A.initialize 5 (always (A.initialize 5 (always False)))) "console"

easyPic : Grid Bool
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
            -- | model.puzzle = Array.set x (Array.set y )
            }

-- VIEW

view : Model -> Html Msg
view model =
    div [style <| boardStyle <| A.length model.puzzle]
        <| List.concat
            [ (drawBoard model.puzzle)
            , [text model.console]
            ]

tileSize : Int
tileSize = 40

drawBoard : Grid Bool -> List (Html Msg)
drawBoard grid =
    Grid.indexedMap (\x y bool -> div [onClick (Click y x), style <| cellStyle bool] [ ]) grid
    |> Grid.toLists
    -- |> List.concat
    |> List.map (\l -> div [style rowStyle] l)

type alias Style = List (String, String)

rowStyle : Style
rowStyle =
    [ ("display", "block")
    , ("margin", "5px 0")
    ]

boardStyle : Int -> Style
boardStyle w =
    [ ("width", px <| (w+1) * tileSize)]

cellStyle : Bool -> Style
cellStyle b =
    [ ("background-color", if b then "black" else "gray")
    , ("width", px tileSize)
    , ("height", px tileSize)
    , ("display", "block")
    , ("float", "left")
    , ("margin", "0.5px")
    ]

px : Int -> String
px d = (toString d) ++ "px"