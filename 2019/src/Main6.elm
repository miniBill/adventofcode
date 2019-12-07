module Main6 exposing (main)

import Dict
import FileProgram
import Html
import String.Extra as String



-- MAIN


main =
    FileProgram.fileMain_ innerView


type Node
    = Node
        { name : String
        , children : List Node
        }


getNode name dict =
    Node
        { name = name
        , children =
            Dict.get name dict
                |> Maybe.withDefault []
                |> List.map (\n -> getNode n dict)
        }


viewNode (Node { name, children }) =
    Html.li []
        [ Html.text name
        , Html.ul [] <| List.map viewNode children
        ]


countOrbits node =
    let
        go d (Node n) =
            n.children
                |> List.map (go (d + 1))
                |> List.sum
                |> (\co -> co + d)
    in
    go 0 node


innerView file =
    file
        |> String.split "\n"
        |> List.map (String.split ")")
        |> List.filterMap
            (\l ->
                case l of
                    [ x, y ] ->
                        Just ( x, y )

                    _ ->
                        Nothing
            )
        |> List.foldl
            (\( center, orbiter ) ->
                Dict.update center
                    (Maybe.withDefault []
                        >> (\l -> Just <| orbiter :: l)
                    )
            )
            Dict.empty
        |> getNode "COM"
        |> countOrbits
        |> String.fromInt
        |> Html.text
