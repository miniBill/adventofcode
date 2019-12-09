module Main exposing (main)

import Browser
import Dict
import Html exposing (Html)
import Html.Attributes as HA
import Random
import Random.List


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Msg =
    List ( Int, Int )


type alias Model =
    List ( Int, Int )


size =
    19


allPos =
    List.range 1 size
        |> List.concatMap
            (\r ->
                List.range 1 size
                    |> List.map (\c -> ( r, c ))
            )


init : flags -> ( Model, Cmd Msg )
init _ =
    ( []
    , List.range 1 1000
        |> List.map (\_ -> Random.generate identity <| Random.List.shuffle allPos)
        |> Cmd.batch
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( List.take 2 msg ++ model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


view : Model -> Html Msg
view model =
    let
        freqs =
            model
                |> List.foldl
                    (\e ->
                        Dict.update e
                            (\v ->
                                v
                                    |> Maybe.withDefault 0
                                    |> (\x -> Just <| x + 1)
                            )
                    )
                    Dict.empty

        freqsValues =
            Dict.values freqs

        mn =
            freqsValues |> List.minimum |> Maybe.withDefault 0

        mx =
            freqsValues |> List.maximum |> Maybe.withDefault 0

        color v =
            let
                r =
                    (v - mn) * 255 // (mx - mn) |> clamp 0 255

                g =
                    0

                b =
                    255 - r |> clamp 0 255

                toHexDigit d =
                    if d < 10 then
                        String.fromInt d

                    else
                        String.fromChar <| Char.fromCode (d - 10 + Char.toCode 'A')

                toHex n =
                    toHexDigit (n // 16) ++ toHexDigit (n |> modBy 16)
            in
            "#" ++ toHex r ++ toHex g ++ toHex b
    in
    List.range 1 size
        |> List.map
            (\r ->
                List.range 1 size
                    |> List.map
                        (\c ->
                            let
                                v =
                                    Maybe.withDefault 0 <| Dict.get ( r, c ) freqs
                            in
                            Html.td [ HA.style "background-color" <| color v ] [ Html.text <| String.fromInt v ]
                        )
                    |> Html.tr []
            )
        |> Html.table []
