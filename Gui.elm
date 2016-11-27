module Gui exposing (view)

import Html exposing (Html, Attribute, beginnerProgram, p, text, div, input, span, table, tr, td, th)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import String

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
            , th [] <| [div [] (fmtDiv colHints)]
            ]
        (rowHints, colHints) = Rules.markedHints model.puzzle model.progress
        divRows = List.map (div []) (G.toLists <| divGrid model.progress)
        trs = List.map2 makeTr rowHints divRows
    in
        div []
            [ boardTable
            , div [] [text model.console]
            -- , p [] [text "Cols: ", text <| String.join " " cols]
            ]

makeTr : List Hint -> Html.Html Msg -> Html.Html Msg
makeTr hints divRow = tr []
        [ td [] [text (List.map (\(num, kind) -> toString num) hints |> String.join " ")]
        , td [] [divRow]
        ]

fmt : List (List Hint) -> List String
fmt hints = List.map (\h -> List.map (\(num, kind) -> toString num) h |> String.join " ") hints

fmtDiv : List (List Hint) -> List (Html.Html Msg)
fmtDiv hints =
    let
        strings = fmt hints
    in
        List.map (\s -> span [style numStyle] [text s]) strings

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
