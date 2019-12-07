module Extra.Intcode5 exposing (StepOutput, run, runString, viewOutput)

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
    | EndOfInput
    | InvalidParameterMode Int
    | AssertionFailed String


intcodeErrorToString : IntcodeError -> String
intcodeErrorToString e =
    case e of
        OutOfRange i ->
            "Index " ++ String.fromInt i ++ " is out of range"

        UnknownOpcode i ->
            "Opcode " ++ String.fromInt i ++ " not implemented (yet?)"

        EndOfInput ->
            "Expecting more input"

        InvalidParameterMode mode ->
            "Invalid parameter mode: " ++ String.fromInt mode

        AssertionFailed err ->
            "Assertion failed: " ++ err


type alias State =
    { memory : Memory
    , ip : IP
    , parameterMode : Int
    , input : List Int
    , output : List Int
    }


type alias StepOutput a =
    { result : Result IntcodeError a
    , state : State
    , trace :
        List
            { previousState : State
            , operation : String
            }
    }


type alias Intcode a =
    State -> StepOutput a


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


pure : (State -> Result IntcodeError a) -> Intcode a
pure x state =
    { result = x state
    , state = state
    , trace = []
    }


liftValue : Result IntcodeError a -> Intcode a
liftValue result =
    pure <| \_ -> result


success : a -> Intcode a
success x =
    pure <| \_ -> Ok x


get : IP -> Intcode Int
get p =
    pure <|
        \{ memory } ->
            Array.get p memory
                |> Result.fromMaybe (OutOfRange p)


set : IP -> Int -> Intcode ()
set p v ({ memory } as state) =
    { result = Ok ()
    , state = { state | memory = Array.set p v memory }
    , trace =
        [ { previousState = state
          , operation = "set " ++ String.fromInt p ++ " " ++ String.fromInt v
          }
        ]
    }


{-| Reads the memory pointed by the current instruction pointer and then increments the instruction pointer
-}
pop : Intcode Int
pop ({ ip } as state) =
    { result =
        Array.get ip state.memory
            |> Result.fromMaybe (OutOfRange ip)
    , state = { state | ip = ip + 1 }
    , trace =
        [ { previousState = state
          , operation = "pop"
          }
        ]
    }


assertTrue : Bool -> String -> Intcode ()
assertTrue check err =
    liftValue <|
        if check then
            Ok ()

        else
            Err <| AssertionFailed err


runString : List Int -> String -> StepOutput ()
runString input program =
    program
        |> String.split ","
        |> List.filterMap String.toInt
        |> Array.fromList
        |> run input


run : List Int -> Array Int -> StepOutput ()
run input memory =
    runProgram
        { memory = memory
        , ip = 0
        , parameterMode = 0
        , input = input
        , output = []
        }


readParameter : Intcode Int
readParameter =
    map2 Tuple.pair
        pop
        popParameterMode
        |> andThen
            (\( pv, mode ) ->
                case mode of
                    0 ->
                        get pv

                    1 ->
                        success pv

                    _ ->
                        liftValue <| Err <| InvalidParameterMode mode
            )


readPointer : Intcode Int
readPointer =
    popParameterMode
        |> andThen (\mode -> assertTrue (modBy 10 mode == 0) "Parameters that an instruction writes to will never be in immediate mode.")
        |> andThen (\_ -> pop)


add : Intcode ()
add =
    map3 (\l r tp -> ( l, r, tp ))
        readParameter
        readParameter
        readPointer
        |> andThen (\( l, r, tp ) -> set tp (l + r))


mult : Intcode ()
mult =
    map3 (\l r tp -> ( l, r, tp ))
        readParameter
        readParameter
        readPointer
        |> andThen (\( l, r, tp ) -> set tp (l * r))


read : Intcode ()
read =
    let
        rawInput ({ input } as state) =
            case input of
                head :: tail ->
                    { result = Ok head
                    , state = { state | input = tail }
                    , trace =
                        [ { previousState = state
                          , operation = "input"
                          }
                        ]
                    }

                [] ->
                    { result = Err EndOfInput
                    , state = state
                    , trace = []
                    }
    in
    readPointer
        |> andThen
            (\p ->
                rawInput
                    |> andThen (set p)
            )


iif : Bool -> a -> a -> a
iif c t f =
    if c then
        t

    else
        f


write : Intcode ()
write =
    let
        rawOutput v ({ output } as state) =
            { result = Ok ()
            , state = { state | output = v :: output }
            , trace =
                [ { previousState = state
                  , operation = "output"
                  }
                ]
            }
    in
    readParameter
        |> andThen rawOutput


setIp : IP -> Intcode ()
setIp ip state =
    { result = Ok ()
    , state = { state | ip = ip }
    , trace =
        [ { previousState = state
          , operation = "setIp " ++ String.fromInt ip
          }
        ]
    }


jumpIfTrue : Intcode ()
jumpIfTrue =
    map2 Tuple.pair
        readParameter
        readParameter
        |> andThen
            (\( v, ip ) ->
                if v /= 0 then
                    setIp ip

                else
                    success ()
            )


jumpIfFalse : Intcode ()
jumpIfFalse =
    map2 Tuple.pair
        readParameter
        readParameter
        |> andThen
            (\( v, ip ) ->
                if v == 0 then
                    setIp ip

                else
                    success ()
            )


lessThan : Intcode ()
lessThan =
    map3 (\l r p -> ( l, r, p ))
        readParameter
        readParameter
        readParameter
        |> andThen
            (\( l, r, p ) -> set p <| iif (l < r) 1 0)


equals : Intcode ()
equals =
    map3 (\l r p -> ( l, r, p ))
        readParameter
        readParameter
        readParameter
        |> andThen
            (\( l, r, p ) -> set p <| iif (l == r) 1 0)


halt : Intcode ()
halt state =
    { result = Ok ()
    , state = state
    , trace =
        [ { previousState = state
          , operation = "halt"
          }
        ]
    }


setParameterMode : Int -> Intcode ()
setParameterMode mode state =
    { result = Ok ()
    , state = { state | parameterMode = mode }
    , trace =
        [ { previousState = state
          , operation = "parameter mode: " ++ String.fromInt mode
          }
        ]
    }


popParameterMode : Intcode Int
popParameterMode ({ parameterMode } as state) =
    { result = Ok (modBy 10 parameterMode)
    , state = { state | parameterMode = parameterMode // 10 }
    , trace =
        [ { previousState = state
          , operation = "pop parameter mode (" ++ String.fromInt (modBy 10 parameterMode) ++ ")"
          }
        ]
    }


runOpcode : Int -> Intcode ()
runOpcode opcode =
    if opcode == 99 then
        halt

    else
        let
            opcodeRunner =
                case modBy 100 opcode of
                    1 ->
                        add

                    2 ->
                        mult

                    3 ->
                        read

                    4 ->
                        write

                    5 ->
                        jumpIfTrue

                    6 ->
                        jumpIfFalse

                    7 ->
                        lessThan

                    8 ->
                        equals

                    _ ->
                        liftValue <| Err (UnknownOpcode opcode)
        in
        setParameterMode (opcode // 100)
            |> andThen (\_ -> opcodeRunner)
            |> andThen (\_ -> runProgram)


runProgram : Intcode ()
runProgram =
    pop
        |> andThen runOpcode


viewOutput : StepOutput a -> Html msg
viewOutput output =
    let
        resultString =
            case output.result of
                Ok _ ->
                    "OK"

                Err e ->
                    intcodeErrorToString e

        viewState state =
            state.memory
                |> Array.map String.fromInt
                |> Array.toList
                |> List.indexedMap
                    (\i v ->
                        if i == state.ip then
                            "[" ++ v ++ "]"

                        else
                            v
                    )
                |> String.join ", "
                |> Html.text

        viewTraceStep { previousState, operation } =
            [ Html.text operation
            , Html.br [] []
            , viewState previousState
            ]

        trace =
            ([ viewState output.state ] :: List.map viewTraceStep output.trace)
                |> List.intersperse [ Html.br [] [], Html.br [] [] ]
                |> List.concat
    in
    Html.span [ HA.style "font-family" "\"Fira Code\"" ] <|
        [ Html.text <| "Result: " ++ resultString
        , Html.br [] []
        , Html.text <| "Output: " ++ String.join ", " (List.reverse <| List.map String.fromInt output.state.output)
        , Html.br [] []
        , Html.text "Trace (newest to oldest): "
        , Html.br [] []
        ]
            ++ trace



-- FUNCTOR


map : (a -> b) -> Intcode a -> Intcode b
map f a =
    success f
        |> andMap a



-- APPLICATIVE FUNCTOR


andMap : Intcode a -> Intcode (a -> b) -> Intcode b
andMap x f =
    f |> andThen (\f_ -> x |> andThen (\x_ -> success <| f_ x_))


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
