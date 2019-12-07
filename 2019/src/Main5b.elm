module Main5b exposing (main)

import Array
import Extra.Intcode5 as Intcode
import FileProgram
import Html
import Html.Attributes as Html
import Html.Events as Html



-- MAIN


main =
    FileProgram.fileMain "5" innerView


innerView input file =
    let
        execution =
            case String.toInt input of
                Nothing ->
                    Html.text <| "Invalid number: " ++ input

                Just int ->
                    file
                        |> Intcode.runString [ int ]
                        |> Intcode.viewOutput
    in
    Html.div []
        [ Html.input [ Html.onInput identity, Html.value input ] []
        , execution
        ]
