module Timer.Timer exposing (init, start, Timer, toString, subscriptions, Msg, update, State(..), pause, state, substractTime, addEvent)

import Task
import Time exposing (Time, millisecond)


type Msg
    = Tick


type alias Timer msg =
    { events : List ( Time, msg )
    , state : State
    , remaining : Time
    }


type State
    = Running
    | Paused
    | Done


timerStep : Time
timerStep =
    100 * millisecond


init : Time -> Timer msg
init time =
    Timer [] Paused time


update : Msg -> Timer msg -> ( Timer msg, List msg )
update message timer =
    case message of
        Tick ->
            let
                newTimer =
                    if timer.state == Running then
                        tickTimer timer
                    else
                        timer

                timeHasPassed =
                    (Tuple.first >> (<=) timer.remaining)

                eventsToApply =
                    List.filter timeHasPassed timer.events

                remainingEvents =
                    List.filter (timeHasPassed >> not) timer.events
            in
                ( { newTimer | events = remainingEvents }, eventsToApply |> List.map Tuple.second )


tickTimer : Timer msg -> Timer msg
tickTimer timer =
    let
        remaining =
            timer.remaining - timerStep |> max 0

        newState =
            if remaining <= 0 then
                Done
            else
                Running
    in
        { timer | remaining = remaining, state = newState }


start : Timer msg -> Timer msg
start model =
    if model.state == Paused then
        { model | state = Running }
    else
        model


pause : Timer msg -> Timer msg
pause model =
    if model.state == Running then
        { model | state = Paused }
    else
        model


state : Timer msg -> State
state timer =
    timer.state


substractTime : Timer msg -> Time -> Timer msg
substractTime timer time =
    { timer | remaining = timer.remaining - time |> max 0 }


toString : Timer msg -> String
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


subscriptions : Timer msg -> Sub Msg
subscriptions timer =
    case timer.state of
        Running ->
            Time.every timerStep (always Tick)

        _ ->
            Sub.none


addEvent : ( Time, msg ) -> Timer msg -> Timer msg
addEvent event timer =
    { timer | events = event :: timer.events }


sendMsg : msg -> Cmd msg
sendMsg msg =
    Task.succeed msg
        |> Task.perform identity
