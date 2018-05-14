module Scenario.Scenario
    exposing
        ( Model
        , ScenarioElement
        , fromScenarioData
        , ScenarioData
        , ElementState(..)
        , Element(..)
        , FinalState(..)
        , Reward(..)
        , State(..)
        , start
        , view
        , Msg
        , subscriptions
        , update
        )

import Attempt exposing (AttemptPenalty(RemoveTime), AttemptStatus(Correct, Incorrect))
import Code.Code as Code
import FeatherIcons
import Graph exposing (Edge, Graph, Node, NodeContext)
import Html exposing (Html, button, div, fieldset, form, form, h2, input, label, legend, p, span, text)
import Html.Attributes exposing (for, id, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Machine.Machine as Machine
import Styles.Styles exposing (defaultButtonClasses)
import Svg.Attributes exposing (height)
import Tachyons exposing (classes)
import Tachyons.Classes exposing (b__black_10, ba, bg_animate, bg_black_70, bg_blue, bg_green, bg_white, black_80, bn, border_box, br2_ns, br__left_ns, br__right_ns, button_reset, center, cf, clip, f2, f3, f4_ns, f5, f5_l, f6, fl, flex, h1, h3, hover_bg_black, input_reset, items_center, justify_center, lh_solid, lh_title, ma0, mb3, mb5, mw7, pa0, pa3, pa4, pointer, pv3, tc, tl, w1, w2, w3, w4, w_100, w_20_l, w_25_m, w_60_m, w_70_l, w_75_m, w_80_l, white)
import Time exposing (Time)
import Timer.Timer as Timer exposing (Timer)


type alias Model msg =
    { state : State
    , failedAttempts : Int
    , screen : Screen
    , name : String
    , scenarioElements : ScenarioElements
    , timer : Timer msg
    , penaltyOnFailedAttempt : Int -> AttemptPenalty
    }


type Screen
    = MainScreen
    | MachineSelectionScreen String
    | MachineScreen Int Machine.Model
    | ElementDoneScreen Int ScenarioElement
    | MachineNotFoundScreen String
    | CodeTypingScreen Code.Model


type Msg
    = Pause
    | Resume
    | BackToMainScreen
    | GoToMachineSelection
    | GoToCodeTyping
    | MachineMsg Machine.Msg
    | CodeMsg Code.Msg
    | TimerMsg Timer.Msg
    | SelectMachine String
    | OnMachineIdInput String


type alias ScenarioElement =
    { state : ElementState
    , finalState : FinalState
    , rewards : List Reward
    , successText : String
    , element : Element
    }


type alias ScenarioElements =
    Graph ScenarioElement ()


type FinalState
    = Final
    | NotFinal


type Element
    = Code String
    | Machine Machine.Model


type ElementState
    = ToDo
    | Done


type State
    = NotStarted
    | Running
    | Paused
    | Won
    | Lost


type alias ScenarioData a =
    { name : String
    , scenarioElements : ScenarioElements
    , timer : Timer a
    , penaltyOnFailedAttempt : Int -> AttemptPenalty
    }


type Reward
    = Modifier Int
    | Time Time


fromScenarioData : ScenarioData a -> Model a
fromScenarioData scenarioData =
    Model NotStarted 0 MainScreen scenarioData.name scenarioData.scenarioElements scenarioData.timer scenarioData.penaltyOnFailedAttempt


update : Msg -> Model msg -> Model msg
update msg model =
    case ( model.state, msg ) of
        ( Running, TimerMsg message ) ->
            let
                ( newTimer, _ ) =
                    Timer.update message model.timer
            in
                { model | timer = newTimer }

        ( Running, Pause ) ->
            { model | state = Paused, timer = Timer.pause model.timer }

        ( Paused, Resume ) ->
            { model | state = Running, timer = Timer.start model.timer }

        ( Running, GoToMachineSelection ) ->
            if model.screen == MainScreen then
                { model | screen = MachineSelectionScreen "" }
            else
                model

        ( Running, GoToCodeTyping ) ->
            if model.screen == MainScreen then
                { model | screen = CodeTypingScreen Code.init }
            else
                model

        ( Running, BackToMainScreen ) ->
            { model | screen = MainScreen }

        ( Won, BackToMainScreen ) ->
            { model | screen = MainScreen }

        ( Running, MachineMsg message ) ->
            case model.screen of
                MachineScreen machineId machine ->
                    updateMachine machineId machine message model

                _ ->
                    model

        ( Running, CodeMsg message ) ->
            case model.screen of
                CodeTypingScreen codeModel ->
                    updateCode codeModel message model

                _ ->
                    model

        ( Running, SelectMachine machineIdString ) ->
            let
                machineMaybe =
                    String.toInt machineIdString
                        |> Result.toMaybe
                        |> Maybe.andThen (getMachineFromId model.scenarioElements)
            in
                case machineMaybe of
                    Just ( machineId, machine ) ->
                        ({ model | screen = MachineScreen machineId machine })

                    Nothing ->
                        ({ model | screen = MachineNotFoundScreen machineIdString })

        ( Running, OnMachineIdInput machineIdString ) ->
            case model.screen of
                MachineSelectionScreen _ ->
                    { model | screen = MachineSelectionScreen machineIdString }

                _ ->
                    model

        _ ->
            model


start : Model msg -> Model msg
start model =
    let
        startedTimer =
            Timer.start model.timer
    in
        { model | state = Running, timer = startedTimer }


resume : Model msg -> Model msg
resume model =
    let
        startedTimer =
            Timer.start model.timer
    in
        if model.state == NotStarted || model.state == Paused then
            { model | state = Running, timer = startedTimer }
        else
            model


updateMachine : Int -> Machine.Model -> Machine.Msg -> Model msg -> Model msg
updateMachine machineId machine message model =
    let
        ( updatedMachine, attemptMaybe ) =
            Machine.update message machine
    in
        case attemptMaybe of
            Just Incorrect ->
                let
                    failedAttempts =
                        model.failedAttempts + 1

                    penalty =
                        model.penaltyOnFailedAttempt failedAttempts

                    timer =
                        case penalty of
                            RemoveTime time ->
                                Timer.substractTime model.timer time
                in
                    { model | screen = MachineScreen machineId updatedMachine, failedAttempts = failedAttempts, timer = timer }

            Just Correct ->
                let
                    updatedScenarioElements =
                        Graph.update machineId flagNodeContextAsDone model.scenarioElements

                    updatedScenarioElementMaybe =
                        Graph.get machineId updatedScenarioElements
                            |> Maybe.map (.node >> .label)

                    newScreen =
                        updatedScenarioElementMaybe
                            |> Maybe.map (ElementDoneScreen machineId)
                            |> Maybe.withDefault MainScreen

                    finalState =
                        updatedScenarioElementMaybe
                            |> Maybe.map .finalState
                            |> Maybe.withDefault NotFinal

                    scenarioState =
                        if finalState == Final then
                            Won
                        else
                            model.state
                in
                    { model | screen = newScreen, scenarioElements = updatedScenarioElements, state = scenarioState }

            Nothing ->
                { model | screen = MachineScreen machineId updatedMachine }


updateCode : Code.Model -> Code.Msg -> Model msg -> Model msg
updateCode codeModel codeMsg model =
    let
        ( newCodeModel, codeMaybe ) =
            Code.update codeMsg codeModel

        validCodeIdMaybe =
            codeMaybe |> Maybe.andThen (findCodeIdInElements model.scenarioElements)
    in
        case ( codeMaybe, validCodeIdMaybe ) of
            ( Just code, Just codeId ) ->
                let
                    updatedScenarioElements =
                        Graph.update codeId flagNodeContextAsDone model.scenarioElements

                    updatedScreen =
                        Graph.get

                    finalState =
                        Graph.get codeId updatedScenarioElements
                            |> Maybe.map (.node >> .label >> .finalState)
                            |> Maybe.withDefault NotFinal

                    scenarioState =
                        if finalState == Final then
                            Won
                        else
                            model.state
                in
                    { model
                        | screen = MainScreen
                        , scenarioElements = updatedScenarioElements
                        , state = scenarioState
                    }

            ( Just code, Nothing ) ->
                let
                    failedAttempts =
                        model.failedAttempts + 1

                    updatedTimer =
                        model.penaltyOnFailedAttempt failedAttempts
                            |> (\(RemoveTime time) -> Timer.substractTime model.timer time)
                in
                    { model
                        | screen = CodeTypingScreen newCodeModel
                        , failedAttempts = failedAttempts
                        , timer = updatedTimer
                    }

            _ ->
                { model | screen = CodeTypingScreen newCodeModel }


view : Model msg -> Html Msg
view model =
    case model.state of
        NotStarted ->
            div [] [ text "The scenario is not started yet" ]

        Running ->
            viewScreen model

        Paused ->
            div []
                [ p [] [ text "The game is paused" ]
                , div [] [ button [ classes defaultButtonClasses, onClick Resume ] [ FeatherIcons.play |> FeatherIcons.toHtml [] ] ]
                ]

        Won ->
            div [ classes [ f2 ] ] [ text "You've won, congratulations!" ]

        _ ->
            div [ classes [] ] [ text "Not running" ]


viewScreen : Model msg -> Html Msg
viewScreen model =
    div []
        [ h2 [ classes [ f2, lh_title ] ] [ text model.name ]
        , p [] [ "Remaining: " ++ Timer.toString model.timer |> text ]
        , case model.screen of
            MainScreen ->
                viewMainScreen

            MachineScreen machineId machine ->
                viewMachine machineId machine

            MachineSelectionScreen machineId ->
                viewMachineSelectionScreen machineId

            CodeTypingScreen askCodeModel ->
                viewCodeTypingScreen askCodeModel

            ElementDoneScreen elementId element ->
                viewElementDoneScreen elementId element

            MachineNotFoundScreen machineIdString ->
                viewMachineNotFoundScreen machineIdString
        ]


viewMainScreen : Html Msg
viewMainScreen =
    div []
        [ div [] [ button [ classes defaultButtonClasses, onClick GoToMachineSelection ] [ FeatherIcons.settings |> FeatherIcons.toHtml [ height "15px" ], text "Machine" ] ]
        , div [] [ button [ classes defaultButtonClasses, onClick GoToCodeTyping ] [ FeatherIcons.lock |> FeatherIcons.toHtml [ height "15px" ], text "Code" ] ]
        , div [] [ button [ classes defaultButtonClasses, onClick Pause ] [ FeatherIcons.pause |> FeatherIcons.toHtml [] ] ]
        ]


flagNodeAsDone : Node ScenarioElement -> Node ScenarioElement
flagNodeAsDone node =
    flagElementAsDone node.label
        |> Node node.id


flagNodeContextAsDone : Maybe (NodeContext ScenarioElement ()) -> Maybe (NodeContext ScenarioElement ())
flagNodeContextAsDone nodeContextMaybe =
    nodeContextMaybe
        |> Maybe.map
            (\nodeContext ->
                { nodeContext | node = flagNodeAsDone nodeContext.node }
            )


flagElementAsDone : ScenarioElement -> ScenarioElement
flagElementAsDone scenarioElement =
    { scenarioElement | state = Done }


viewMachine : Int -> Machine.Model -> Html Msg
viewMachine machineId machine =
    div []
        [ Machine.view machineId machine |> Html.map MachineMsg
        , backToMainScreenButton
        ]


viewMachineSelectionScreen : String -> Html Msg
viewMachineSelectionScreen machineId =
    div []
        [ form [ onSubmit (SelectMachine machineId), classes [ mw7, center, pa4, br2_ns, ba, b__black_10, bg_blue, mb3 ] ]
            [ fieldset [ classes [ cf, bn, ma0, pa0 ] ]
                [ legend [ classes [ pa0, f5, f4_ns, mb3, white, tl ] ] [ text "Enter your machine code" ]
                , div
                    [ classes [ cf ] ]
                    [ label [ classes [ clip ], for "machineId" ] [ text "Machine code" ]
                    , input [ classes [ border_box, bn, f6, f5_l, input_reset, fl, black_80, bg_white, pa3, lh_solid, w_100, w_75_m, w_80_l, br2_ns, br__left_ns ], id "machineId", value machineId, onInput OnMachineIdInput ] []
                    , input [ type_ "submit", classes [ f6, f5_l, button_reset, fl, pv3, tc, bn, bg_animate, bg_black_70, hover_bg_black, white, pointer, w_100, w_25_m, w_20_l, br2_ns, br__right_ns ] ] [ text "Show machine" ]
                    ]
                ]
            ]
        , backToMainScreenButton
        ]


viewCodeTypingScreen : Code.Model -> Html Msg
viewCodeTypingScreen askCodeModel =
    div []
        [ Code.askCodeView askCodeModel |> Html.map CodeMsg
        , backToMainScreenButton
        ]


viewMachineNotFoundScreen : String -> Html Msg
viewMachineNotFoundScreen machineIdString =
    div []
        [ "Machine " ++ machineIdString ++ " not found" |> text
        , backToMainScreenButton
        ]


viewElementDoneScreen : Int -> ScenarioElement -> Html Msg
viewElementDoneScreen elementId scenarioElement =
    let
        rewardsView =
            List.map viewReward scenarioElement.rewards
    in
        div []
            [ p [ classes [ f3 ] ] [ text "Congrats!" ]
            , p [] [ text scenarioElement.successText ]
            , div [ classes [ mb3 ] ] rewardsView
            , backToMainScreenButton
            ]


viewReward : Reward -> Html Msg
viewReward reward =
    case reward of
        Modifier value ->
            div [ classes [ w3, h3, bg_blue, white, flex, items_center, justify_center, f3, center ] ] [ "+" ++ toString value |> text ]

        Time time ->
            div [ classes [ w4, h3, bg_green, white, flex, items_center, justify_center, f3, center ] ] [ "+" ++ (toString (Time.inSeconds time)) ++ "s" |> text ]


backToMainScreenButton : Html Msg
backToMainScreenButton =
    div []
        [ button [ classes defaultButtonClasses, onClick BackToMainScreen ] [ FeatherIcons.x |> FeatherIcons.toHtml [] ]
        ]


findCodeIdInElements : ScenarioElements -> String -> Maybe Int
findCodeIdInElements scenarioElements code =
    Graph.nodes scenarioElements
        |> List.filter (.label >> .element >> (==) (Code code))
        |> List.head
        |> Maybe.map .id


getMachineFromId : ScenarioElements -> Int -> Maybe ( Int, Machine.Model )
getMachineFromId scenarioElements machineId =
    Graph.get machineId scenarioElements
        |> Maybe.map (.node >> .label)
        |> Maybe.andThen extractMachine
        |> Maybe.map (\machine -> ( machineId, machine ))


extractMachine : ScenarioElement -> Maybe Machine.Model
extractMachine scenarioElement =
    case scenarioElement.element of
        Machine machine ->
            Just machine

        _ ->
            Nothing


subscriptions : Model msg -> Sub Msg
subscriptions model =
    if model.state == Running then
        Timer.subscriptions model.timer
            |> Sub.map TimerMsg
    else
        Sub.none
