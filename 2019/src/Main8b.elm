module Main8b exposing (main)

import FileProgram
import Html
import List.Extra as List



-- MAIN


main =
    FileProgram.fileMain_ innerView


width =
    25


height =
    6


innerView file =
    let
        layers =
            file
                |> String.replace "\n" ""
                |> String.toList
                |> List.greedyGroupsOf (width * height)
                |> List.map (List.greedyGroupsOf width)

        numberToChar c =
            case c of
                '2' ->
                    '_'

                '0' ->
                    'O'

                '1' ->
                    'X'

                _ ->
                    c

        viewLayer layer =
            layer
                |> List.map (List.map numberToChar >> String.fromList >> Html.text)
                |> List.intersperse (Html.br [] [])

        layersView =
            layers
                |> List.map viewLayer
                |> List.intersperse separator
                |> List.concat

        separator =
            [ Html.br [] [], Html.br [] [] ]

        transparent =
            List.range 1 height |> List.map (\_ -> List.range 1 width |> List.map (\_ -> '2'))

        combined =
            layers |> List.foldl (List.map2 combineRow) transparent

        combineRow =
            List.map2 combineCell

        combineCell e a =
            if a == '2' then
                e

            else
                a
    in
    Html.div [] (layersView ++ separator ++ viewLayer combined)
