module Main7b exposing (main)

import Extra.Intcode7 as Intcode
import FileProgram
import Html
import Html.Attributes as Html
import Html.Events as Html
import List.Extra as List



-- MAIN


main =
    FileProgram.fileMain ( "4,3,2,1,0", False ) innerView


innerView ( input, showTrace ) file =
    let
        bestPhases =
            let
                phases =
                    List.range 0 4
            in
            phases
                |> List.concatMap
                    (\a ->
                        phases
                            |> List.filter (\b -> a /= b)
                            |> List.concatMap
                                (\b ->
                                    phases
                                        |> List.filter (\c -> a /= c && b /= c)
                                        |> List.concatMap
                                            (\c ->
                                                phases
                                                    |> List.filter (\d -> a /= d && b /= d && c /= d)
                                                    |> List.concatMap
                                                        (\d ->
                                                            phases
                                                                |> List.filter (\e -> a /= e && b /= e && c /= e && d /= e)
                                                                |> List.map
                                                                    (\e ->
                                                                        { a = a
                                                                        , b = b
                                                                        , c = c
                                                                        , d = d
                                                                        , e = e
                                                                        , output = [ 100 ] --directOutput a b c d e
                                                                        }
                                                                    )
                                                        )
                                            )
                                )
                    )
                |> List.sortBy .output
                |> List.reverse
                |> List.take 3
                |> List.map
                    (\{ a, b, c, d, e, output } ->
                        Html.div []
                            [ Html.text <|
                                "a: "
                                    ++ String.fromInt a
                                    ++ ", b: "
                                    ++ String.fromInt b
                                    ++ ", c: "
                                    ++ String.fromInt c
                                    ++ ", d: "
                                    ++ String.fromInt d
                                    ++ ", e: "
                                    ++ String.fromInt e
                                    ++ ", output: "
                                    ++ String.join ", " (List.map String.fromInt output)
                            ]
                    )
                |> Html.div []

        execution =
            case input |> String.split "," |> List.filterMap String.toInt of
                [ a, b, c, d, e ] ->
                    let
                        run label s =
                            file
                                |> Intcode.runString s
                                |> (\o ->
                                        ( Html.div [ Html.style "display" "inline-block" ]
                                            [ Html.h1 [] [ Html.text <| "AMP " ++ label ]
                                            , Intcode.viewOutput { showTrace = showTrace } o
                                            ]
                                        , []
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
                    Html.div []
                        [ atrace
                        , btrace
                        , ctrace
                        , dtrace
                        , etrace
                        ]

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
        , Html.br [] []
        , Html.br [] []
        , bestPhases
        ]
