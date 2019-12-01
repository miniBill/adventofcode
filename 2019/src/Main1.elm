module Main1 exposing (main)

import FileProgram
import Html



-- MAIN


main =
    FileProgram.fileMain innerView


innerView file =
    file
        |> String.split "\n"
        |> List.filterMap String.toInt
        |> List.map (\c -> c // 3 - 2)
        |> List.sum
        |> String.fromInt
        |> Html.text
