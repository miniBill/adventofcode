module Main6b exposing (main)

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


getPrefix needle (Node { name, children }) =
    if needle == name then
        Just []

    else
        children
            |> List.map (getPrefix needle)
            |> List.filterMap identity
            |> List.head
            |> Maybe.map (\l -> name :: l)


countJumps root =
    let
        youprefix =
            getPrefix "YOU" root
                |> Maybe.withDefault []

        sanprefix =
            getPrefix "SAN" root
                |> Maybe.withDefault []

        commonprefix =
            List.map2 Tuple.pair youprefix sanprefix
                |> List.filterMap
                    (\( l, r ) ->
                        if l == r then
                            Just l

                        else
                            Nothing
                    )
    in
    List.length youprefix + List.length sanprefix - 2 * List.length commonprefix


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
        |> countJumps
        |> String.fromInt
        |> Html.text
