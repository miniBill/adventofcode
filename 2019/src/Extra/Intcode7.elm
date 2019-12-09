module Extra.Intcode7 exposing (InnerError, StepEffect(..), StepResult, init, initString, step, toStepResult, viewStepResult)

import Array exposing (Array)
import Html exposing (Html)
import Html.Attributes as HA
import List.Extra as List


type alias IP =
    Int


type alias Memory =
    Array Int


type InnerError
    = UnknownOpcode Int
    | OutOfRange Int
    | InvalidParameterMode Int
    | AssertionFailed String


intcodeErrorToString : InnerError -> String
intcodeErrorToString e =
    case e of
        OutOfRange i ->
            "Index " ++ String.fromInt i ++ " is out of range"

        UnknownOpcode i ->
            "Opcode " ++ String.fromInt i ++ " not implemented (yet?)"

        InvalidParameterMode mode ->
            "Invalid parameter mode: " ++ String.fromInt mode

        AssertionFailed err ->
            "Assertion failed: " ++ err


type alias State =
    { memory : Memory
    , ip : IP
    , highlight : Maybe IP
    , parametersMode : Int
    }


type alias StepResult =
    { effect : StepEffect
    , state : State
    , log : List String
    }


type StepEffect
    = None
    | Error InnerError
    | Input (Int -> StepResult)
    | Output Int


type alias Step =
    State -> StepResult


type alias Inner a =
    State -> { result : Result InnerError a, state : State, log : List String }


toStepResult : State -> StepResult
toStepResult state =
    { effect = None
    , state = state
    , log = []
    }


read : Step
read state =
    let
        rp =
            readPointer state
    in
    case rp.result of
        Ok p ->
            { effect =
                Input
                    (\v ->
                        { effect = None
                        , state =
                            { state
                                | memory = Array.set p v rp.state.memory
                                , highlight = Just p
                            }
                        , log = [ "input -> " ++ String.fromInt v ]
                        }
                    )
            , state = rp.state
            , log = rp.log
            }

        Err e ->
            { effect = Error e
            , state = rp.state
            , log = rp.log
            }


write : Step
write state =
    let
        rp =
            readParameter state
    in
    case rp.result of
        Ok p ->
            { effect = Output p
            , state = rp.state
            , log = ("output " ++ String.fromInt p) :: rp.log
            }

        Err e ->
            { effect = Error e
            , state = rp.state
            , log = rp.log
            }


popParameterMode : Inner ParameterMode
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
    { result = mode
    , state = { state | parametersMode = parametersMode // 10 }
    , log = [ "pop parameter mode -> " ++ String.fromInt raw ++ " = " ++ either intcodeErrorToString parameterModeToString mode ]
    }


either : (e -> b) -> (x -> b) -> Result e x -> b
either err ok r =
    case r of
        Err e ->
            err e

        Ok o ->
            ok o


get : IP -> Inner Int
get p ({ memory } as state) =
    let
        result =
            Array.get p memory |> Result.fromMaybe (OutOfRange p)
    in
    { result = result
    , state = state
    , log = [ "get " ++ String.fromInt p ++ " -> " ++ either intcodeErrorToString String.fromInt result ]
    }


set : IP -> Int -> Inner ()
set p v ({ memory } as state) =
    { result = Ok ()
    , state =
        { state
            | memory = Array.set p v memory
            , highlight = Just p
        }
    , log = [ "set " ++ String.fromInt p ++ " " ++ String.fromInt v ]
    }


{-| Reads the memory pointed by the current instruction pointer and then increments the instruction pointer
-}
pop : Inner Int
pop ({ ip } as state) =
    let
        result =
            Array.get ip state.memory |> Result.fromMaybe (OutOfRange ip)
    in
    { result = result
    , log = [ "pop -> " ++ either intcodeErrorToString String.fromInt result ]
    , state = { state | ip = ip + 1 }
    }


assertTrue : Bool -> String -> Inner ()
assertTrue check err state =
    { result =
        if check then
            Ok ()

        else
            Err <| AssertionFailed err
    , state = state
    , log = []
    }


initString : String -> State
initString program =
    program
        |> String.replace "\n" ""
        |> String.split ","
        |> List.filterMap String.toInt
        |> Array.fromList
        |> init


init : Array Int -> State
init memory =
    { memory = memory
    , ip = 0
    , parametersMode = 0
    , highlight = Nothing
    }


toStep : Inner () -> Step
toStep inner state =
    let
        r =
            inner state
    in
    { effect = either Error (\() -> None) r.result
    , state = r.state
    , log = r.log
    }


step : Step
step state =
    let
        popped =
            pop state
    in
    case popped.result of
        Err e ->
            { effect = Error e, state = popped.state, log = popped.log }

        Ok opcode ->
            if opcode == 99 then
                halt popped.state

            else
                let
                    pstate =
                        popped.state

                    modeSet =
                        { pstate | parametersMode = opcode // 100 }

                    ( opcodename, opcodeStep ) =
                        case modBy 100 opcode of
                            1 ->
                                ( "add", toStep add )

                            2 ->
                                ( "mult", toStep mult )

                            3 ->
                                ( "input", read )

                            4 ->
                                ( "output", write )

                            5 ->
                                ( "jump-if-true", toStep jumpIfTrue )

                            6 ->
                                ( "jump-if-false", toStep jumpIfFalse )

                            7 ->
                                ( "less than", toStep lessThan )

                            8 ->
                                ( "equals", toStep equals )

                            _ ->
                                ( "ERR", \s -> { effect = Error (UnknownOpcode opcode), state = s, log = [] } )

                    done =
                        opcodeStep modeSet
                in
                { effect = done.effect
                , log =
                    [ "opcode: " ++ opcodename
                    , "set parameters mode " ++ String.fromInt (opcode // 100)
                    ]
                        ++ done.log
                , state = done.state
                }


type ParameterMode
    = Position
    | Immediate


readParameter : Inner Int
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


readPointer : Inner Int
readPointer =
    popParameterMode
        |> andThen (\mode -> assertTrue (mode == Position) "Parameters that an instruction writes to will never be in immediate mode.")
        |> andThen (\_ -> pop)


add : Inner ()
add =
    map3 (\l r tp -> ( l, r, tp ))
        readParameter
        readParameter
        readPointer
        |> log (\( l, r, _ ) -> String.fromInt l ++ " + " ++ String.fromInt r ++ " = " ++ String.fromInt (l + r))
        |> andThen (\( l, r, tp ) -> set tp (l + r))


mult : Inner ()
mult =
    map3 (\l r tp -> ( l, r, tp ))
        readParameter
        readParameter
        readPointer
        |> log (\( l, r, _ ) -> String.fromInt l ++ " * " ++ String.fromInt r ++ " = " ++ String.fromInt (l * r))
        |> andThen (\( l, r, tp ) -> set tp (l * r))


iif : Bool -> a -> a -> a
iif c t f =
    if c then
        t

    else
        f


setIp : IP -> Inner ()
setIp ip state =
    { result = Ok ()
    , state = { state | ip = ip }
    , log = [ "setIp " ++ String.fromInt ip ]
    }


jumpIfTrue : Inner ()
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


jumpIfFalse : Inner ()
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


lessThan : Inner ()
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


equals : Inner ()
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


halt : Step
halt state =
    { effect = None
    , state = state
    , log = [ "halt" ]
    }


parameterModeToString : ParameterMode -> String
parameterModeToString mode =
    case mode of
        Immediate ->
            "Immediate"

        Position ->
            "Position"


viewStepResult : StepResult -> Html msg
viewStepResult result =
    let
        effectString =
            case result.effect of
                None ->
                    "None"

                Error e ->
                    intcodeErrorToString e

                Input _ ->
                    "Waiting input"

                Output o ->
                    "Output: " ++ String.fromInt o

        memoryGroupSize =
            4

        viewState s =
            s.memory
                |> Array.map (String.fromInt >> String.padLeft 4 '0')
                |> Array.toList
                |> List.indexedMap
                    (\i v ->
                        v
                            |> Html.text
                            |> (\h ->
                                    if i == s.ip then
                                        Html.u [] [ h ]

                                    else
                                        h
                               )
                            |> (\h ->
                                    if Just i == s.highlight then
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
                                    ++ String.fromInt s.parametersMode
                             , Html.br [] []
                             ]
                                ++ blocks
                            )
                   )
    in
    Html.div
        [ HA.style "font-family" "\"Fira Code\"" ]
    <|
        [ Html.text <| "Effect: " ++ effectString
        , Html.br [] []
        , Html.br [] []
        , Html.text "State:"
        , Html.br [] []
        , Html.br [] []
        , viewState result.state
        ]



-- WRITE


log : (a -> String) -> Inner a -> Inner a
log toMsg x state =
    let
        r =
            x state
    in
    case r.result of
        Ok v ->
            { r | log = r.log ++ [ toMsg v ] }

        Err _ ->
            r



-- FUNCTOR


map : (a -> b) -> Inner a -> Inner b
map f a =
    success f
        |> andMap a



-- APPLICATIVE FUNCTOR


success : a -> Inner a
success x state =
    { result = Ok x
    , state = state
    , log = []
    }


andMap : Inner a -> Inner (a -> b) -> Inner b
andMap x f =
    f |> andThen (\f_ -> x |> andThen (\x_ -> success <| f_ x_))


map2 : (a -> b -> c) -> Inner a -> Inner b -> Inner c
map2 f a b =
    success f
        |> andMap a
        |> andMap b


map3 : (a -> b -> c -> d) -> Inner a -> Inner b -> Inner c -> Inner d
map3 f a b c =
    success f
        |> andMap a
        |> andMap b
        |> andMap c



-- MONAD


andThen : (a -> Inner b) -> Inner a -> Inner b
andThen f x state =
    let
        x_ =
            x state
    in
    case x_.result of
        Ok v ->
            let
                f_ =
                    f v x_.state
            in
            { f_ | log = x_.log ++ f_.log }

        Err e ->
            { result = Err e
            , log = x_.log
            , state = x_.state
            }
