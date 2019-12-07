module Main4 exposing (main)

import Html
import Set



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
                |> Set.fromList
                |> Set.size
                |> (/=) 6

        isIncreasing =
            lst
                |> List.foldl (\e ( last, acc ) -> ( e, acc && Char.toCode e >= Char.toCode last )) ( '/', True )
                |> Tuple.second
    in
    hasDuplicates && isIncreasing


generate =
    List.range 146810 612564
