module Extra.Intcode5 exposing (StepOutput, run, viewOutput)

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


run : List Int -> Array Int -> StepOutput ()
run input memory =
    runProgram
        { memory = memory
        , ip = 0
        , input = input
        , output = []
        }


type ParameterMode
    = Position
    | Immediate


readParameter : ParameterMode -> Intcode Int
readParameter mode =
    pop
        |> andThen
            (case mode of
                Position ->
                    get

                Immediate ->
                    success
            )


toMode : Int -> Result IntcodeError ParameterMode
toMode m =
    case m of
        0 ->
            Ok Position

        1 ->
            Ok Immediate

        _ ->
            Err <| InvalidParameterMode m


getParameterModes1 : Int -> Result IntcodeError ParameterMode
getParameterModes1 =
    modBy 10 >> toMode


getParameterModes2 : Int -> Result IntcodeError ( ParameterMode, ParameterMode )
getParameterModes2 modes =
    Result.map2 Tuple.pair
        (getParameterModes1 modes)
        (getParameterModes1 <| modes // 10)


getParameterModes3 : Int -> Result IntcodeError ( ParameterMode, ParameterMode, ParameterMode )
getParameterModes3 modes =
    Result.map3 (\m1 m2 m3 -> ( m1, m2, m3 ))
        (getParameterModes1 <| modes)
        (getParameterModes1 <| modes // 10)
        (getParameterModes1 <| modes // 100)


outputParamsMustBeInPointerMode : String
outputParamsMustBeInPointerMode =
    "Parameters that an instruction writes to will never be in immediate mode."


add : Int -> Intcode ()
add modes =
    getParameterModes3 modes
        |> liftValue
        |> andThen
            (\( mode1, mode2, mode3 ) ->
                assertTrue (mode3 == Position) outputParamsMustBeInPointerMode
                    |> andThen
                        (\_ ->
                            map3 (\l r tp -> ( l, r, tp ))
                                (readParameter mode1)
                                (readParameter mode2)
                                pop
                                |> andThen (\( l, r, tp ) -> set tp (l + r))
                        )
            )


mult : Int -> Intcode ()
mult modes =
    getParameterModes3 modes
        |> liftValue
        |> andThen
            (\( mode1, mode2, mode3 ) ->
                assertTrue (mode3 == Position) outputParamsMustBeInPointerMode
                    |> andThen
                        (\_ ->
                            map3 (\l r tp -> ( l, r, tp ))
                                (readParameter mode1)
                                (readParameter mode2)
                                pop
                                |> andThen (\( l, r, tp ) -> set tp (l * r))
                        )
            )


read : Int -> Intcode ()
read modes =
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
    getParameterModes1 modes
        |> liftValue
        |> andThen (\mode1 -> assertTrue (mode1 == Position) outputParamsMustBeInPointerMode)
        |> andThen
            (\_ ->
                pop
                    |> andThen
                        (\p ->
                            rawInput
                                |> andThen (set p)
                        )
            )


write : Int -> Intcode ()
write modes =
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
    getParameterModes1 modes
        |> liftValue
        |> andThen
            (\mode1 ->
                readParameter mode1
                    |> andThen rawOutput
            )


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


runOpcode : Int -> Intcode ()
runOpcode opcode =
    if opcode == 99 then
        halt

    else
        andThen (\_ -> runProgram) <|
            case modBy 100 opcode of
                1 ->
                    add (opcode // 100)

                2 ->
                    mult (opcode // 100)

                3 ->
                    read (opcode // 100)

                4 ->
                    write (opcode // 100)

                _ ->
                    liftValue <| Err (UnknownOpcode opcode)


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
