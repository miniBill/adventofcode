module Main5 exposing (main)

import Array
import Extra.Intcode5 as Intcode
import FileProgram



-- MAIN


main =
    FileProgram.fileMain_ innerView


innerView file =
    file
        |> String.split ","
        |> List.filterMap String.toInt
        |> Array.fromList
        |> Intcode.run [ 1 ]
        |> Intcode.viewOutput
