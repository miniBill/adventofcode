module Main12 exposing (main)

import Html exposing (Html)


type alias Position a =
    { a
        | x : Int
        , y : Int
        , z : Int
    }


type alias Velocity a =
    { a
        | vx : Int
        , vy : Int
        , vz : Int
    }


type alias Planet =
    Position (Velocity {})


type alias State =
    List Planet


viewPlanet : Planet -> ( Html msg, Int )
viewPlanet { x, y, z, vx, vy, vz } =
    let
        pot =
            abs x + abs y + abs z

        kin =
            abs vx + abs vy + abs vz

        tot =
            pot * kin

        html =
            Html.text <|
                String.concat
                    [ "pos=<x="
                    , String.fromInt x
                    , ", y="
                    , String.fromInt y
                    , ", z="
                    , String.fromInt z
                    , ">, vel=<x="
                    , String.fromInt vx
                    , ", y="
                    , String.fromInt vy
                    , ", z="
                    , String.fromInt vz
                    , ">, pot="
                    , String.fromInt pot
                    , ", kin="
                    , String.fromInt kin
                    , ", tot="
                    , String.fromInt tot
                    ]
    in
    ( html, tot )


viewState : State -> List (Html msg)
viewState s =
    let
        ( planetViews, energies ) =
            s
                |> List.map viewPlanet
                |> List.unzip

        totalView =
            Html.text <| "Total energy: " ++ String.fromInt (List.sum energies)

        children =
            planetViews
                |> (\pv -> pv ++ [ totalView ])
                |> List.intersperse (Html.br [] [])
    in
    children


deltaToInt : Int -> Int -> Int
deltaToInt this that =
    if that < this then
        -1

    else if that > this then
        1

    else
        0


step : State -> State
step planets =
    let
        gravity planet =
            let
                go prop =
                    planets
                        |> List.map (\other -> deltaToInt (prop planet) (prop other))
                        |> List.sum

                vx =
                    go .x

                vy =
                    go .y

                vz =
                    go .z
            in
            { planet
                | vx = planet.vx + vx
                , vy = planet.vy + vy
                , vz = planet.vz + vz
            }

        move ({ x, y, z, vx, vy, vz } as planet) =
            { planet
                | x = x + vx
                , y = y + vy
                , z = z + vz
            }
    in
    planets
        |> List.map gravity
        |> List.map move


run : Int -> State -> List State
run steps state =
    if steps <= 0 then
        [ state ]

    else
        let
            state_ =
                step state
        in
        state :: run (steps - 1) state_


init : List (Position a) -> List Planet
init =
    List.map
        (\{ x, y, z } ->
            { x = x
            , y = y
            , z = z
            , vx = 0
            , vy = 0
            , vz = 0
            }
        )


main : Html msg
main =
    [ { x = -2, y = 9, z = -5 }
    , { x = 16, y = 19, z = 9 }
    , { x = 0, y = 3, z = 6 }
    , { x = 11, y = 0, z = 11 }
    ]
        |> init
        |> run 1000
        |> List.map viewState
        |> List.intersperse [ Html.br [] [], Html.br [] [] ]
        |> List.concat
        |> Html.node "tt" []
