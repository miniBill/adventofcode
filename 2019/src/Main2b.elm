module Main2b exposing (main)

import Array
import Extra.Intcode as Intcode
import FileProgram
import Html



-- MAIN


main =
    FileProgram.fileMain_ innerView


innerView file =
    let
        runnv n v =
            file
                |> String.split ","
                |> List.filterMap String.toInt
                |> Array.fromList
                |> Array.set 1 n
                |> Array.set 2 v
                |> Intcode.run
                |> .state
                |> .memory
                |> Array.get 0
                |> Maybe.withDefault -999
                |> String.fromInt
                |> Html.text
    in
    List.range 0 99
        |> List.map
            (\row ->
                Html.tr []
                    (List.range 0 99
                        |> List.map
                            (\column ->
                                Html.td [] [ Html.text <| "N: " ++ String.fromInt row, Html.text <| ", V: " ++ String.fromInt column ++ " = ", runnv row column ]
                            )
                    )
            )
        |> Html.table []
