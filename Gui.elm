module Gui exposing (view)

import Html exposing (Html, Attribute, beginnerProgram, text, div, input, span, table, tr, td, th)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)

import Core exposing (..)
import Grid as G
import Rules

view : Model -> Html Msg
view model =
    let
        boardTable = table [] (firstRow::trs)
        firstRow = tr []
            [ th [] []
            , th [] <| List.map (\num -> div [style numStyle] [text num]) (List.map toString cols)
            ]
        (rows, cols) = Rules.gridHeaders model.puzzle
        trs = List.map2 (\divRow nums -> tr [] (tds (toString nums) divRow)) (G.toLists <| divGrid model.progress) rows
        tds header row = [td [] [text header], td [] row]
    in
        div []
            [ boardTable
            , div [] [text model.console]
            ]

numberedDivGrid : G.Grid Bool -> G.Grid Bool -> List (Html Msg)
numberedDivGrid puzzle progress =
    let
        base = divGrid progress
        (cols, rows) = Rules.gridHeaders puzzle
        numToDiv nums = div [] [(text <| toString nums)]
        annotate row nums = div [style rowStyle] ((numToDiv nums)::row)
    in
        List.map2 annotate (G.toLists base) cols

tileSize : Int
tileSize = 40

divGrid : G.Grid Bool -> G.Grid (Html Msg)
divGrid = G.indexedMap (\x y bool -> div [onClick (Click y x), style <| cellStyle bool] [ ])

cellDivGrid : G.Grid Bool -> List (Html Msg)
cellDivGrid grid =
    divGrid grid
    |> G.toLists
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

numStyle : Style
numStyle =
    [ ("background-color", "white")
    , ("width", px tileSize)
    , ("display", "block")
    , ("float", "left")
    , ("margin", "0.5px")
    ]

numCell : Style
numCell =
    [
    ]

px : Int -> String
px d = (toString d) ++ "px"