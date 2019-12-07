module Main3b exposing (main)

import Dict exposing (Dict)
import FileProgram
import Html



-- MAIN


main =
    FileProgram.fileMain_ innerView


type Wire
    = First
    | Second


type Cell
    = One Wire Int
    | Both Int


type alias Memory =
    Dict ( Int, Int ) Cell


addWire : Wire -> String -> Memory -> Memory
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

                inner a ( pos, d, m ) =
                    let
                        m_ =
                            Dict.update pos
                                (\v ->
                                    Just <|
                                        case v of
                                            Nothing ->
                                                One wire d

                                            Just found ->
                                                case found of
                                                    Both _ ->
                                                        found

                                                    One foundWire foundDistance ->
                                                        if wire == foundWire then
                                                            found

                                                        else
                                                            Both (d + foundDistance)
                                )
                                m

                        pos_ =
                            direction pos
                    in
                    if a <= 0 then
                        ( pos, d, m_ )

                    else
                        inner (a - 1) ( pos_, d + 1, m_ )
            in
            inner amount

        go r c d mem =
            List.foldl step ( ( r, c ), d, mem )
    in
    input
        |> String.split ","
        |> go 0 0 0 memory
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

        viewCell w =
            case w of
                Nothing ->
                    "."

                Just (One First _) ->
                    "f"

                Just (One Second _) ->
                    "s"

                Just (Both _) ->
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
                                                viewCell <| Dict.get ( row, col ) memory
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
                                    |> Dict.filter (\( r, c ) _ -> r /= 0 || c /= 0)
                                    |> Dict.values
                                    |> List.filterMap
                                        (\v ->
                                            case v of
                                                Both d ->
                                                    Just d

                                                One _ _ ->
                                                    Nothing
                                        )
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
