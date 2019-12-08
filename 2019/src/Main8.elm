module Main8 exposing (main)

import FileProgram
import Html
import List.Extra as List



-- MAIN


main =
    FileProgram.fileMain_ innerView


innerView file =
    file
        |> String.replace "\n" ""
        |> String.toList
        |> List.greedyGroupsOf (25 * 6)
        |> List.sortBy (\g -> g |> List.filter ((==) '0') |> List.length)
        |> List.take 1
        |> List.map (\g -> Html.text <| String.fromInt <| List.length (List.filter ((==) '1') g) * List.length (List.filter ((==) '2') g))
        |> Html.div []
