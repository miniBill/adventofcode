module Main1b exposing (main)

import FileProgram
import Html



-- MAIN


main =
    FileProgram.fileMain innerView


innerView file =
    file
        |> String.split "\n"
        |> List.filterMap String.toInt
        |> List.map fuelRequired
        |> List.sum
        |> String.fromInt
        |> Html.text


fuelRequired : Int -> Int
fuelRequired x =
    if x >= 9 then
        let
            raw =
                x // 3 - 2
        in
        raw + fuelRequired raw

    else
        0
