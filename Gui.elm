module Gui exposing (view)

import Html exposing (Html, Attribute, beginnerProgram, p, text, div, input, span, table, tr, td, th)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)

import Core exposing (..)
import Grid as G
import Rules

view : Model -> Html Msg
view model =
    let
        boardTable = -- the table contains trs
            table [] (firstRow::trs)
        firstRow = tr []
            [ th [] []
            , th [] <| List.map (\num -> div [style numStyle] [text num]) cols
            ]
        (rows, cols) = Rules.gridHeaders model.puzzle
        trs = -- The trs contain tds
            List.map2
                (\divRow nums -> tr [] (tds (nums) divRow))
                (G.toLists <| divGrid model.progress)
                rows
        tds header row = -- The tds contain numbers, then divs.
            [ td [] [text header]
            , td [] row
            ]
    in
        div []
            [ boardTable
            , div [] [text model.console]
            -- , p [] [text "Cols: ", text <| String.join " " cols]
            ]

tileSize : Int
tileSize = 40

divGrid : G.Grid Mark -> G.Grid (Html Msg)
divGrid = G.indexedMap (\x y mark -> div [onClick (Click y x), style <| cellStyle mark] [])

type alias Style = List (String, String)

cellStyle : Mark -> Style
cellStyle m =
    [ ("width", px tileSize)
    , ("height", px tileSize)
    , ("display", "block")
    , ("float", "left")
    , ("margin", "0.5px")
    ] ++ case m of
        White ->
            [ ("background-color", "lightgray") ]
        Black ->
            [ ("background-color", "black") ]
        Dot ->
            [ ("background", "linear-gradient(135deg, lightgray, lightgray 40%, gray, lightgray 60%, lightgray)") ]

numStyle : Style
numStyle =
    [ ("background-color", "white")
    , ("width", px tileSize)
    , ("display", "block")
    , ("float", "left")
    , ("margin", "0.5px")
    ]

px : Int -> String
px d = (toString d) ++ "px"
