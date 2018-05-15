module Timer.Timer exposing (init, start, Timer, toString, subscriptions, Msg, update, State(..), pause, state, substractTime)

import Time exposing (Time, millisecond)


type Msg
    = Tick


type alias Timer =
    { state : State
    , elapsed : Time
    , remaining : Time
    }


type State
    = Running
    | Paused
    | Done


type alias TimerStatus =
    { elapsed : Time
    , remaining : Time
    }


timerStep : Time
timerStep =
    100 * millisecond


init : Time -> Timer
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


substractTime : Timer -> Time -> Timer
substractTime timer time =
    { timer | remaining = timer.remaining - time |> max 0 }


toString : Timer -> String
toString timer =
    let
        inMinutes =
            Time.inMinutes timer.remaining |> floor

        inSeconds =
            timer.remaining
                - Time.minute
                * (toFloat inMinutes)
                |> Time.inSeconds
                |> floor

        inMinutesString =
            if inMinutes > 0 then
                Basics.toString inMinutes
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
            Basics.toString inSeconds
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
            Time.every timerStep (always Tick)

        _ ->
            Sub.none
