module Main3 exposing (main)

import Dict exposing (Dict)
import FileProgram
import Html



-- MAIN


main =
    FileProgram.fileMain_ innerView


type Cell
    = First
    | Second
    | Both


type alias Memory =
    Dict ( Int, Int ) Cell


addWire : Cell -> String -> Memory -> Memory
addWire wire input memory =
    let
        step cmd =
            let
                parseDirection dir ( r, c ) =
                    case dir of
                        'U' ->
                            ( r - 1, c )

                        'D' ->
                            ( r + 1, c )

                        'L' ->
                            ( r, c - 1 )

                        'R' ->
                            ( r, c + 1 )

                        _ ->
                            ( r, c )

                ( direction, amount ) =
                    String.uncons cmd
                        |> Maybe.andThen (\( d, am ) -> Maybe.map (Tuple.pair d) (String.toInt am))
                        |> Maybe.map (Tuple.mapFirst parseDirection)
                        |> Maybe.withDefault ( identity, 0 )

                inner a ( r, c, m ) =
                    let
                        m_ =
                            Dict.update ( r, c )
                                (\v ->
                                    Just <|
                                        case v of
                                            Nothing ->
                                                wire

                                            Just found ->
                                                if found == wire then
                                                    wire

                                                else
                                                    Both
                                )
                                m

                        ( r_, c_ ) =
                            direction ( r, c )
                    in
                    if a <= 0 then
                        ( r, c, m_ )

                    else
                        inner (a - 1) ( r_, c_, m_ )
            in
            inner amount

        go r c mem =
            List.foldl step ( r, c, mem )
    in
    input
        |> String.split ","
        |> go 0 0 memory
        |> (\( _, _, mem ) -> mem)


viewGrid : Memory -> Html.Html a
viewGrid memory =
    let
        rows =
            memory |> Dict.keys |> List.map Tuple.first

        minrow =
            rows |> List.minimum |> Maybe.withDefault 0

        maxrow =
            rows |> List.maximum |> Maybe.withDefault 0

        cols =
            memory |> Dict.keys |> List.map Tuple.second

        mincol =
            cols |> List.minimum |> Maybe.withDefault 0

        maxcol =
            cols |> List.maximum |> Maybe.withDefault 0

        viewWire w =
            case w of
                Nothing ->
                    "."

                Just First ->
                    "f"

                Just Second ->
                    "s"

                Just Both ->
                    "X"
    in
    Html.table []
        (List.range minrow maxrow
            |> List.map
                (\row ->
                    Html.tr []
                        (List.range mincol maxcol
                            |> List.map
                                (\col ->
                                    Html.td []
                                        [ Html.text <|
                                            if row == 0 && col == 0 then
                                                "O"

                                            else
                                                viewWire <| Dict.get ( row, col ) memory
                                        ]
                                )
                        )
                )
        )


innerView file =
    file
        |> String.split "\n"
        |> (\lst ->
                case lst of
                    first :: second :: _ ->
                        let
                            added =
                                Dict.empty
                                    |> addWire First first
                                    |> addWire Second second

                            result =
                                added
                                    |> Dict.filter (\( r, c ) v -> v == Both && (r /= 0 || c /= 0))
                                    |> Dict.keys
                                    |> List.map (\( r, c ) -> abs r + abs c)
                                    |> List.minimum
                                    |> Maybe.withDefault 0
                                    |> String.fromInt
                        in
                        Html.span []
                            [ {- viewGrid added
                                 ,
                              -}
                              Html.text <| "Result: " ++ result
                            ]

                    _ ->
                        Html.text "Input should have two lines"
           )
