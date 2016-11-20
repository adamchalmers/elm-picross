module Gui exposing (view)

import Html exposing (Html, Attribute, beginnerProgram, text, div, input, span)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Array as A

import Core exposing (..)
import Grid

view : Model -> Html Msg
view model =
    div [style <| boardStyle <| A.length model.puzzle]
        <| List.concat
            [ (drawBoard model.progress)
            , [text model.console]
            ]

tileSize : Int
tileSize = 40

drawBoard : Grid.Grid Bool -> List (Html Msg)
drawBoard grid =
    Grid.indexedMap (\x y bool -> div [onClick (Click y x), style <| cellStyle bool] [ ]) grid
    |> Grid.toLists
    |> List.map (div [style rowStyle])

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