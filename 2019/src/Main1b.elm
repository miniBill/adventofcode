module Main1b exposing (main)

import FileProgram
import Html



-- MAIN


main =
    FileProgram.fileMain_ innerView


innerView file =
    file
        |> String.split "\n"
        |> List.filterMap String.toInt
        |> List.map fuelRequired
        |> List.sum
        |> String.fromInt
        |> Html.text


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
