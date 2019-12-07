module Main5b exposing (main)

import Array
import Extra.Intcode5 as Intcode
import FileProgram
import Html
import Html.Attributes as Html
import Html.Events as Html



-- MAIN


main =
    FileProgram.fileMain ( "5", False ) innerView


innerView ( input, showTrace ) file =
    let
        execution =
            case String.toInt input of
                Nothing ->
                    Html.text <| "Invalid number: " ++ input

                Just int ->
                    file
                        |> Intcode.runString [ int ]
                        |> Intcode.viewOutput { showTrace = showTrace }
    in
    Html.div []
        [ Html.text "Input: "
        , Html.input [ Html.onInput (\i -> ( i, showTrace )), Html.value input ] []
        , Html.br [] []
        , Html.label []
            [ Html.input
                [ Html.type_ "checkbox"
                , Html.onCheck (\s -> ( input, s ))
                , Html.checked showTrace
                ]
                []
            , Html.text "Show trace"
            ]
        , Html.br [] []
        , Html.br [] []
        , execution
        ]
