module Extra.ClosedFormFor1b exposing (main)

import Html exposing (Html)
import Html.Attributes as Html
import List.Extra as List


main : Html msg
main =
    let
        mn =
            0

        mx =
            1000

        raw =
            List.map fuelRequired <|
                List.range mn mx

        column rows =
            Html.div
                [ Html.style "display" "inline-block"
                , Html.style "margin-left" "1em"
                , Html.style "margin-top" "1em"
                ]
                (List.intersperse (Html.br [] []) rows)
    in
    [ List.range mn mx
    , raw
    , diffs raw
    ]
        |> List.transpose
        |> List.map (List.map intToHtml)
        |> (\l -> List.map Html.text [ "Mass", "Fuel", "Difference" ] :: l)
        |> List.map column
        |> Html.div
            [ Html.style "margin-bottom" "1em"
            , Html.style "margin-right" "1em"
            ]


intToHtml i =
    let
        ( fg, bg ) =
            case modBy 10 i of
                0 ->
                    ( "black", "#FF" )

                1 ->
                    ( "black", "#FDD" )

                2 ->
                    ( "black", "#DFD" )

                3 ->
                    ( "black", "#DDF" )

                4 ->
                    ( "black", "#FFD" )

                5 ->
                    ( "black", "#FDF" )

                6 ->
                    ( "black", "#DFF" )

                7 ->
                    ( "black", "#DDD" )

                8 ->
                    ( "white", "#666" )

                _ ->
                    ( "white", "#000" )
    in
    Html.span
        [ Html.style "color" fg
        , Html.style "background-color" bg
        , Html.style "padding-left" "10px"
        , Html.style "padding-right" "10px"
        , Html.style "margin" "0"
        ]
        [ Html.text <| String.fromInt i ]


diffs : List Int -> List Int
diffs ls =
    let
        fold a l =
            case l of
                [] ->
                    []

                x :: xs ->
                    (x - a) :: fold x xs
    in
    case ls of
        [] ->
            []

        x :: ys ->
            fold x ys


fuelRequired : Int -> Int
fuelRequired mass =
    if mass >= 9 then
        let
            raw =
                mass // 3 - 2
        in
        raw + fuelRequired raw

    else
        0
