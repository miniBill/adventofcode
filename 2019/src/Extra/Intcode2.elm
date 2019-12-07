module Extra.Intcode2 exposing (IntcodeOutput, run, viewOutput)

import Array exposing (Array)
import Html exposing (Html)
import Html.Attributes as HA


type alias IP =
    Int


type alias Memory =
    Array Int


type IntcodeError
    = UnknownOpcode Int
    | OutOfRange Int


intcodeErrorToString : IntcodeError -> String
intcodeErrorToString e =
    case e of
        OutOfRange i ->
            "Index " ++ String.fromInt i ++ " is out of range"

        UnknownOpcode i ->
            "Opcode " ++ String.fromInt i ++ " not implemented (yet?)"


type alias State =
    { memory : Memory
    , ip : IP
    }


type alias IntcodeOutput a =
    { result : Result IntcodeError a
    , state : State
    , trace : List State
    }


type alias Intcode a =
    State -> IntcodeOutput a


andThen : (a -> Intcode b) -> Intcode a -> Intcode b
andThen f x s =
    let
        x_ =
            x s
    in
    case x_.result of
        Ok v ->
            let
                fx_ =
                    f v x_.state
            in
            { result = fx_.result
            , state = fx_.state
            , trace = fx_.trace ++ x_.trace
            }

        Err e ->
            { result = Err e
            , state = x_.state
            , trace = x_.trace
            }


success : a -> Intcode a
success x { memory, ip } =
    { result = Ok x
    , state = { ip = ip, memory = memory }
    , trace = []
    }


andMap : Intcode a -> Intcode (a -> b) -> Intcode b
andMap x f =
    f |> andThen (\f_ -> x |> andThen (\x_ -> success <| f_ x_))


map : (a -> b) -> Intcode a -> Intcode b
map f a =
    success f
        |> andMap a


map2 : (a -> b -> c) -> Intcode a -> Intcode b -> Intcode c
map2 f a b =
    success f
        |> andMap a
        |> andMap b


map3 : (a -> b -> c -> d) -> Intcode a -> Intcode b -> Intcode c -> Intcode d
map3 f a b c =
    success f
        |> andMap a
        |> andMap b
        |> andMap c


getIP : Intcode IP
getIP ({ memory, ip } as state) =
    { result = Ok ip
    , state = state
    , trace = []
    }


get : IP -> Intcode Int
get p ({ memory, ip } as state) =
    { result =
        Array.get p memory
            |> Result.fromMaybe (OutOfRange p)
    , state = state
    , trace = []
    }


set : IP -> Int -> Intcode ()
set p v ({ memory, ip } as state) =
    { result = Ok ()
    , state = { ip = ip, memory = Array.set p v memory }
    , trace = [ state ]
    }


{-| Reads the memory pointed by the current instruction pointer and then increments the instruction pointer
-}
pop : Intcode Int
pop ({ memory, ip } as state) =
    { result =
        Array.get ip memory
            |> Result.fromMaybe (OutOfRange ip)
    , state = { state | ip = ip + 1 }
    , trace = [ state ]
    }


uknownOpcode : Int -> Intcode a
uknownOpcode inst state =
    { result = Err (UnknownOpcode inst)
    , state = state
    , trace = []
    }


run : Array Int -> IntcodeOutput ()
run memory =
    runProgram { memory = memory, ip = 0 }


runProgram : Intcode ()
runProgram =
    let
        add =
            map3 (\l r tp -> ( l, r, tp ))
                (pop |> andThen get)
                (pop |> andThen get)
                pop
                |> andThen (\( l, r, tp ) -> set tp (l + r))
                |> andThen (\() -> runProgram)

        mult =
            map3 (\l r tp -> ( l, r, tp ))
                (pop |> andThen get)
                (pop |> andThen get)
                pop
                |> andThen (\( l, r, tp ) -> set tp (l * r))
                |> andThen (\() -> runProgram)

        halt =
            success ()
    in
    pop
        |> andThen
            (\inst ->
                case inst of
                    1 ->
                        add

                    2 ->
                        mult

                    99 ->
                        halt

                    _ ->
                        uknownOpcode inst
            )


viewOutput : IntcodeOutput a -> Html msg
viewOutput output =
    let
        resultString =
            case output.result of
                Ok _ ->
                    "OK"

                Err e ->
                    intcodeErrorToString e

        trace =
            (output.state :: output.trace)
                |> List.map
                    (\snapshot ->
                        snapshot.memory
                            |> Array.map String.fromInt
                            |> Array.toList
                            |> List.indexedMap
                                (\i v ->
                                    if i == snapshot.ip then
                                        "[" ++ v ++ "]"

                                    else
                                        v
                                )
                            |> String.join ", "
                            |> Html.text
                            |> (\t -> [ t ])
                    )
                |> List.intersperse [ Html.br [] [], Html.br [] [] ]
                |> List.concat
    in
    Html.span [ HA.style "font-family" "\"Fira Code\"" ] <|
        [ Html.text <| "Result: " ++ resultString
        , Html.br [] []
        , Html.text "Trace (newest to oldest): "
        , Html.br [] []
        ]
            ++ trace
