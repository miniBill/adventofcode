module Main4b exposing (main)

import Html



-- MAIN


main =
    generate
        |> List.filter check
        |> List.length
        |> String.fromInt
        |> Html.text


check i =
    let
        lst =
            String.fromInt i
                |> String.toList

        hasDuplicates =
            lst
                |> List.foldl
                    (\e ( streakchar, streaklength, won ) ->
                        if won then
                            ( streakchar, streaklength, won )

                        else if streakchar == e then
                            ( e, streaklength + 1, False )

                        else
                            ( e, 1, streaklength == 2 )
                    )
                    ( '/', 1, False )
                |> (\( _, len, won ) -> won || len == 2)

        isIncreasing =
            lst
                |> List.foldl (\e ( last, acc ) -> ( e, acc && Char.toCode e >= Char.toCode last )) ( '/', True )
                |> Tuple.second
    in
    hasDuplicates && isIncreasing


generate =
    List.range 146810 612564
