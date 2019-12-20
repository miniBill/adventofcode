module Main10 exposing (main)

import Dict
import FileProgram
import Html
import Set



-- MAIN


main =
    FileProgram.fileMain_ innerView


innerView file =
    let
        rows =
            file
                |> String.split "\n"

        list =
            rows
                |> List.indexedMap
                    (\r s ->
                        s
                            |> String.toList
                            |> List.indexedMap
                                (\c a ->
                                    if a == '#' then
                                        Just ( r, c )

                                    else
                                        Nothing
                                )
                            |> List.filterMap identity
                    )
                |> List.concat

        set =
            Set.fromList list

        ray dx dy =
            let
                g =
                    gcd (abs dx) (abs dy)
            in
            ( dx // g, dy // g )

        seen r c =
            if Set.member ( r, c ) set then
                list
                    |> List.foldl (\( x, y ) -> Set.insert (ray (x - r) (y - c))) Set.empty
                    |> Set.size
                    |> (\s -> s - 1)

            else
                0

        cols =
            rows
                |> List.head
                |> Maybe.withDefault ""
                |> String.length
                |> (\l -> l - 1)

        seenList : List ( Int, Int, Int )
        seenList =
            List.range 0 (List.length rows - 1)
                |> List.concatMap
                    (\r ->
                        List.range 0 cols
                            |> List.map (\c -> ( r, c, seen r c ))
                    )

        ( bestr, bestc, bestseen ) =
            seenList
                |> List.sortBy (\( _, _, s ) -> -s)
                |> List.head
                |> Maybe.withDefault ( -1, -1, -1 )

        cellLength =
            bestseen
                |> String.fromInt
                |> String.length
                |> (\n ->
                        if n < 2 then
                            n

                        else
                            n + 1
                   )

        grid =
            List.range 0 (List.length rows - 1)
                |> List.map
                    (\r ->
                        List.range 0 cols
                            |> List.map (seen r)
                            |> List.map
                                (\n ->
                                    if n == 0 then
                                        "." |> String.padLeft cellLength '.'

                                    else
                                        String.fromInt n |> String.padLeft cellLength '.'
                                )
                            |> String.concat
                            |> Html.text
                    )
                |> List.intersperse (Html.br [] [])
                |> Html.node "tt" []
    in
    Html.div []
        [ Html.text <| "Best: " ++ String.fromInt bestr ++ "," ++ String.fromInt bestc ++ " = " ++ String.fromInt bestseen
        , Html.br [] []
        , grid
        ]


gcd : Int -> Int -> Int
gcd x y =
    case y of
        0 ->
            x

        _ ->
            gcd y <| remainderBy y x
