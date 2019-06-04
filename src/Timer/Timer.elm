module Timer.Timer exposing (Milliseconds, Msg, State(..), Timer, decoder, init, pause, start, state, subscriptions, substractTime, toString, update)

import Json.Decode as Decode exposing (Decoder)
import Time


type Msg
    = Tick


type alias Timer =
    { state : State
    , elapsed : Milliseconds
    , remaining : Milliseconds
    }


type State
    = Running
    | Paused
    | Done


type alias Milliseconds =
    Int


type alias TimerStatus =
    { elapsed : Milliseconds
    , remaining : Milliseconds
    }


timerStep : Milliseconds
timerStep =
    100


init : Milliseconds -> Timer
init time =
    Timer Paused 0 time


update : Msg -> Timer -> ( Timer, TimerStatus )
update message timer =
    case message of
        Tick ->
            let
                newTimer =
                    if timer.state == Running then
                        tickTimer timer

                    else
                        timer
            in
            ( newTimer, TimerStatus newTimer.elapsed newTimer.remaining )


tickTimer : Timer -> Timer
tickTimer timer =
    let
        remaining =
            timer.remaining - timerStep |> max 0

        elapsed =
            timer.elapsed + timerStep

        newState =
            if remaining <= 0 then
                Done

            else
                Running
    in
    { timer | remaining = remaining, elapsed = elapsed, state = newState }


start : Timer -> Timer
start model =
    if model.state == Paused then
        { model | state = Running }

    else
        model


pause : Timer -> Timer
pause model =
    if model.state == Running then
        { model | state = Paused }

    else
        model


state : Timer -> State
state timer =
    timer.state


substractTime : Timer -> Milliseconds -> Timer
substractTime timer time =
    { timer | remaining = timer.remaining - time |> max 0 }


toString : Timer -> String
toString timer =
    let
        inMinutes =
            timer.remaining // 60000

        inSeconds =
            modBy 60000 timer.remaining // 1000

        inMinutesString =
            if inMinutes > 0 then
                String.fromInt inMinutes
                    ++ " minute"
                    ++ (if inMinutes > 1 then
                            "s"

                        else
                            ""
                       )
                    ++ " "

            else
                ""

        inSecondsString =
            String.fromInt inSeconds
                ++ " second"
                ++ (if inSeconds > 1 then
                        "s"

                    else
                        ""
                   )
    in
    inMinutesString ++ inSecondsString


subscriptions : Timer -> Sub Msg
subscriptions timer =
    case timer.state of
        Running ->
            Time.every (toFloat timerStep) (always Tick)

        _ ->
            Sub.none


decoder : Decoder Timer
decoder =
    Decode.field "initialTimeInSeconds" Decode.int
        |> Decode.map ((*) 1000)
        |> Decode.map init
