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
    div []
        [ gridToTable model
        , div [] [text model.console]
        -- , p [] [text "Cols: ", text <| String.join " " cols]
        ]

gridToTable : Model -> Html Msg
gridToTable model =
    let
        firstRow = tr []
            [ th [] []
            , th [] <| [div [] (fmtDiv colHints)]
            ]
        (rowHints, colHints) = Rules.markedHints model.puzzle model.progress
        divRows = List.map (div []) (G.toLists <| divGrid model.progress)
    in
        div [] [table [] (firstRow::(List.map2 makeTr rowHints divRows))]

makeTr : List Hint -> Html.Html Msg -> Html.Html Msg
makeTr hints divRow = tr []
        [ td [] [text (List.map (\(num, kind) -> toString num) hints |> String.join " ")]
        , td [] [divRow]
        ]

fmtDiv : List (List Hint) -> List (Html.Html Msg)
fmtDiv hints =
    let
        spanFor (num, hintType) = span [style <| numStyle hintType] [text <| toString num]
    in
        List.map (\h -> div [] <| List.map spanFor h) hints

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

numStyle : HintProgress -> Style
numStyle ht =
    [ ("background-color", "white")
    , ("width", px tileSize)
    , ("display", "block")
    , ("float", "left")
    , ("margin", "0.5px")
    , ("color", case ht of
        Todo -> "black"
        Done -> "gray"
        Mistake -> "orange")
    ]

px : Int -> String
px d = (toString d) ++ "px"
