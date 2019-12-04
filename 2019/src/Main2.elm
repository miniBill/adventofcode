module Main2 exposing (main)

import Array
import Extra.Intcode as Intcode
import FileProgram



-- MAIN


main =
    FileProgram.fileMain_ innerView


innerView file =
    file
        |> String.split ","
        |> List.filterMap String.toInt
        |> Array.fromList
        |> Array.set 1 12
        |> Array.set 2 2
        |> Intcode.run
        |> Intcode.viewOutput
