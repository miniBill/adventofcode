module Extra.Intcode5 exposing (StepOutput, run, runString, viewOutput)

import Array exposing (Array)
import Html exposing (Html)
import Html.Attributes as HA
import List.Extra as List


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
    , highlight : Maybe IP
    , parametersMode : Int
    , input : List Int
    , output : List Int
    }


type Trace
    = NextState State
    | Log String


type alias StepOutput a =
    { result : Result IntcodeError a
    , state : State
    , trace : List Trace
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


pipeline : Intcode () -> Intcode a -> Intcode a
pipeline op x =
    x |> andThen (\v -> op |> map (\_ -> v))


get : IP -> Intcode Int
get p =
    pure
        (\{ memory } ->
            Array.get p memory
                |> Result.fromMaybe (OutOfRange p)
        )
        |> log (\r -> "get " ++ String.fromInt p ++ " -> " ++ String.fromInt r)


changeState : (State -> State) -> Intcode a -> Intcode a
changeState f x =
    x
        |> andThen
            (\v state ->
                let
                    state_ =
                        f { state | highlight = Nothing }
                in
                { result = Ok v
                , state = state_
                , trace = [ NextState state_ ]
                }
            )


set : IP -> Int -> Intcode ()
set p v =
    success ()
        |> log (\_ -> "set " ++ String.fromInt p ++ " " ++ String.fromInt v)
        |> changeState
            (\({ memory } as state) ->
                { state
                    | memory = Array.set p v memory
                    , highlight = Just p
                }
            )


either : (e -> b) -> (x -> b) -> Result e x -> b
either err ok r =
    case r of
        Err e ->
            err e

        Ok o ->
            ok o


{-| Reads the memory pointed by the current instruction pointer and then increments the instruction pointer
-}
pop : Intcode Int
pop =
    pure
        (\({ ip } as state) ->
            Array.get ip state.memory
                |> Result.fromMaybe (OutOfRange ip)
        )
        |> log (\v -> "pop -> " ++ String.fromInt v)
        |> changeState (\({ ip } as state) -> { state | ip = ip + 1 })


log : (a -> String) -> Intcode a -> Intcode a
log msg op =
    op
        |> andThen
            (\v state ->
                { result = Ok v
                , state = state
                , trace = [ Log <| msg v ]
                }
            )


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
        |> String.replace "\n" ""
        |> String.split ","
        |> List.filterMap String.toInt
        |> Array.fromList
        |> run input


run : List Int -> Array Int -> StepOutput ()
run input memory =
    success ()
        |> changeState identity
        |> andThen (\_ -> runProgram)
        |> (\f ->
                f
                    { memory = memory
                    , ip = 0
                    , parametersMode = 0
                    , highlight = Nothing
                    , input = input
                    , output = []
                    }
           )


type ParameterMode
    = Position
    | Immediate


readParameter : Intcode Int
readParameter =
    map2 Tuple.pair
        popParameterMode
        pop
        |> andThen
            (\( mode, pv ) ->
                case mode of
                    Position ->
                        get pv

                    Immediate ->
                        success pv
            )


readPointer : Intcode Int
readPointer =
    popParameterMode
        |> andThen (\mode -> assertTrue (mode == Position) "Parameters that an instruction writes to will never be in immediate mode.")
        |> andThen (\_ -> pop)


add : Intcode ()
add =
    map3 (\l r tp -> ( l, r, tp ))
        readParameter
        readParameter
        readPointer
        |> log (\( l, r, tp ) -> String.fromInt l ++ " + " ++ String.fromInt r ++ " = " ++ String.fromInt (l + r))
        |> andThen (\( l, r, tp ) -> set tp (l + r))


mult : Intcode ()
mult =
    map3 (\l r tp -> ( l, r, tp ))
        readParameter
        readParameter
        readPointer
        |> log (\( l, r, tp ) -> String.fromInt l ++ " * " ++ String.fromInt r ++ " = " ++ String.fromInt (l * r))
        |> andThen (\( l, r, tp ) -> set tp (l * r))


iif : Bool -> a -> a -> a
iif c t f =
    if c then
        t

    else
        f


read : Intcode ()
read =
    let
        rawInput ({ input } as state) =
            case input of
                head :: tail ->
                    success head
                        |> log (\_ -> "input -> " ++ String.fromInt head)
                        |> changeState (\_ -> { state | input = tail })
                        |> (\f -> f state)

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


write : Intcode ()
write =
    let
        rawOutput v =
            success ()
                |> log (\_ -> "output " ++ String.fromInt v)
                |> changeState (\({ output } as state) -> { state | output = v :: output })
    in
    readParameter
        |> andThen rawOutput


setIp : IP -> Intcode ()
setIp ip =
    success ()
        |> log (\_ -> "setIp " ++ String.fromInt ip)
        |> changeState (\state -> { state | ip = ip })


jumpIfTrue : Intcode ()
jumpIfTrue =
    map2 Tuple.pair
        readParameter
        readParameter
        |> andThen
            (\( v, ip ) ->
                if v /= 0 then
                    success ()
                        |> log (\_ -> "jump becase " ++ String.fromInt v ++ " /= 0")
                        |> andThen (\_ -> setIp ip)

                else
                    success ()
                        |> log (\_ -> "don't jump becase 0 == 0")
            )


jumpIfFalse : Intcode ()
jumpIfFalse =
    map2 Tuple.pair
        readParameter
        readParameter
        |> andThen
            (\( v, ip ) ->
                if v == 0 then
                    success ()
                        |> log (\_ -> "jump becase 0 == 0")
                        |> andThen (\_ -> setIp ip)

                else
                    success ()
                        |> log (\_ -> "don't jump becase " ++ String.fromInt v ++ " /= 0")
            )


lessThan : Intcode ()
lessThan =
    map3 (\l r p -> ( l, r, p ))
        readParameter
        readParameter
        readPointer
        |> log
            (\( l, r, _ ) ->
                if l < r then
                    String.fromInt l ++ " < " ++ String.fromInt r ++ " -> 1"

                else
                    String.fromInt l ++ " >= " ++ String.fromInt r ++ " -> 0"
            )
        |> andThen
            (\( l, r, p ) -> set p <| iif (l < r) 1 0)


equals : Intcode ()
equals =
    map3 (\l r p -> ( l, r, p ))
        readParameter
        readParameter
        readPointer
        |> log
            (\( l, r, _ ) ->
                if l == r then
                    String.fromInt l ++ " == " ++ String.fromInt r ++ " -> 1"

                else
                    String.fromInt l ++ " /= " ++ String.fromInt r ++ " -> 0"
            )
        |> andThen
            (\( l, r, p ) -> set p <| iif (l == r) 1 0)


halt : Intcode ()
halt =
    success () |> log (\_ -> "halt")


setParameterMode : Int -> Intcode ()
setParameterMode mode =
    success ()
        |> log (\_ -> "set parameters mode " ++ String.fromInt mode)
        |> changeState (\state -> { state | parametersMode = mode })


parameterModeToString : ParameterMode -> String
parameterModeToString mode =
    case mode of
        Immediate ->
            "Immediate"

        Position ->
            "Position"


popParameterMode : Intcode ParameterMode
popParameterMode ({ parametersMode } as state) =
    let
        raw =
            modBy 10 parametersMode

        mode =
            case raw of
                0 ->
                    Ok Position

                1 ->
                    Ok Immediate

                _ ->
                    Err <| InvalidParameterMode raw
    in
    liftValue mode
        |> log (\_ -> "pop parameter mode -> " ++ String.fromInt raw ++ " = " ++ either intcodeErrorToString parameterModeToString mode)
        |> changeState (\s -> { s | parametersMode = parametersMode // 10 })
        |> (\f -> f state)


runOpcode : Int -> Intcode ()
runOpcode opcode =
    if opcode == 99 then
        halt

    else
        let
            ( opcodename, opcodeRunner ) =
                case modBy 100 opcode of
                    1 ->
                        ( "add", add )

                    2 ->
                        ( "mult", mult )

                    3 ->
                        ( "input", read )

                    4 ->
                        ( "output", write )

                    5 ->
                        ( "jump-if-true", jumpIfTrue )

                    6 ->
                        ( "jump-if-false", jumpIfFalse )

                    7 ->
                        ( "less than", lessThan )

                    8 ->
                        ( "equals", equals )

                    _ ->
                        ( "ERR", liftValue <| Err (UnknownOpcode opcode) )
        in
        success ()
            |> log (\_ -> "opcode: " ++ opcodename)
            |> andThen (\_ -> setParameterMode (opcode // 100))
            |> andThen (\_ -> opcodeRunner)
            |> andThen (\_ -> runProgram)


runProgram : Intcode ()
runProgram =
    pop
        |> andThen runOpcode


viewOutput : { showTrace : Bool } -> StepOutput a -> Html msg
viewOutput { showTrace } output =
    let
        resultString =
            case output.result of
                Ok _ ->
                    "OK"

                Err e ->
                    intcodeErrorToString e

        memoryGroupSize =
            4

        viewState state =
            state.memory
                |> Array.map (String.fromInt >> String.padLeft 4 '0')
                |> Array.toList
                |> List.indexedMap
                    (\i v ->
                        v
                            |> Html.text
                            |> (\h ->
                                    if i == state.ip then
                                        Html.u [] [ h ]

                                    else
                                        h
                               )
                            |> (\h ->
                                    if Just i == state.highlight then
                                        Html.b [] [ h ]

                                    else
                                        h
                               )
                    )
                |> List.greedyGroupsOf memoryGroupSize
                |> List.indexedMap
                    (\i gr ->
                        Html.div
                            [ HA.style "display" "inline-block"
                            , HA.style "border" "1px solid black"
                            ]
                        <|
                            List.intersperse (Html.text " ") <|
                                Html.span [ HA.style "color" "gray" ]
                                    [ Html.text <| String.padLeft 4 '0' <| String.fromInt <| i * memoryGroupSize
                                    ]
                                    :: gr
                    )
                |> (\blocks ->
                        Html.div []
                            ([ Html.text <|
                                "Parameters mode: "
                                    ++ String.fromInt state.parametersMode
                                    ++ ", Input: ["
                                    ++ String.join ", " (List.map String.fromInt state.input)
                                    ++ "], Output: ["
                                    ++ String.join ", " (List.map String.fromInt state.output)
                                    ++ "]"
                             , Html.br [] []
                             ]
                                ++ blocks
                            )
                   )

        viewTraceStep step =
            case step of
                Log msg ->
                    [ Html.text msg ]

                NextState previousState ->
                    [ viewState previousState ]

        trace =
            if showTrace then
                ([ viewState output.state ] :: List.map viewTraceStep output.trace)
                    |> List.intersperse [ Html.br [] [], Html.br [] [] ]
                    |> List.concat

            else
                [ Html.text "Disabled" ]
    in
    Html.div
        [ HA.style "font-family" "\"Fira Code\"" ]
    <|
        [ Html.text <| "Result: " ++ resultString
        , Html.br [] []
        , Html.br [] []
        , Html.text <| "Output: " ++ String.join ", " (List.reverse <| List.map String.fromInt output.state.output)
        , Html.br [] []
        , Html.br [] []
        , Html.text "Trace (newest to oldest): "
        , Html.br [] []
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
