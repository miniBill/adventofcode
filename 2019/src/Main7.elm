module Main7 exposing (main)

import Array
import Extra.Intcode5 as Intcode
import FileProgram
import Html
import Html.Attributes as Html
import Html.Events as Html



-- MAIN


main =
    FileProgram.fileMain ( "4,3,2,1,0", False ) innerView


innerView ( input, showTrace ) file =
    let
        execution =
            case input |> String.split "," |> List.filterMap String.toInt of
                [ a, b, c, d, e ] ->
                    let
                        run label s =
                            file
                                |> Intcode.runString s
                                |> (\o ->
                                        ( [ Html.h1 [] [ Html.text <| "AMP " ++ label ]
                                          , Intcode.viewOutput { showTrace = showTrace } o
                                          ]
                                        , o.state.output
                                        )
                                   )

                        ( atrace, aout ) =
                            run "a" [ a, 0 ]

                        ( btrace, bout ) =
                            run "b" <| b :: aout

                        ( ctrace, cout ) =
                            run "c" <| c :: bout

                        ( dtrace, dout ) =
                            run "d" <| d :: cout

                        ( etrace, _ ) =
                            run "e" <| e :: dout
                    in
                    Html.div [] <|
                        atrace
                            ++ btrace
                            ++ ctrace
                            ++ dtrace
                            ++ etrace

                _ ->
                    Html.text "Insert 5 numbers"
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
