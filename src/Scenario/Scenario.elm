module Scenario.Scenario exposing
    ( Element(..)
    , ElementState(..)
    , FinalState(..)
    , Model
    , Msg
    , Reward(..)
    , ScenarioData
    , ScenarioElement
    , State(..)
    , decoder
    , fromScenarioData
    , start
    , subscriptions
    , update
    , view
    )

import Attempt exposing (AttemptPenalty(..), AttemptStatus(..))
import Code.Code as Code
import FeatherIcons
import Graph exposing (AcyclicGraph(..), Edge, Graph, Node, NodeContext)
import Html exposing (Html, button, div, fieldset, form, h2, input, label, legend, p, text)
import Html.Attributes exposing (autocomplete, autofocus, for, id, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import IntDict
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Extra as Decode
import List.Extra as List
import Machine.Machine as Machine exposing (simpleElectricalPanel, waterPipePuzzle)
import Machine.SimpleElectricalPanel exposing (Plug(..))
import Machine.WaterPipePuzzle exposing (Direction(..), OpeningType(..), Option(..), Pipe(..), PipeWithOptions)
import Styles.Styles exposing (defaultButtonClasses)
import Svg.Attributes exposing (height)
import Tachyons exposing (classes)
import Tachyons.Classes exposing (b__black_10, ba, bg_animate, bg_black_70, bg_blue, bg_green, bg_white, black_80, bn, border_box, br2_ns, br__left_ns, br__right_ns, button_reset, center, cf, clip, f2, f3, f4_ns, f5, f5_l, f6, fl, flex, h3, hover_bg_black, input_reset, items_center, justify_center, lh_solid, lh_title, ma0, mb3, mw7, pa0, pa3, pa4, pointer, pv3, tc, tl, w3, w4, w_100, w_20_l, w_25_m, w_75_m, w_80_l, white)
import Timer.Timer as Timer exposing (Timer)


type alias Model =
    { state : State
    , failedAttempts : Int
    , screen : Screen
    , name : String
    , scenarioElements : ScenarioElements
    , timer : Timer
    , penaltyOnFailedAttempt : Int -> AttemptPenalty
    }


type Screen
    = MainScreen
    | MachineSelectionScreen String
    | MachineScreen Int Machine.Model
    | ElementDoneScreen Int ScenarioElement
    | MachineNotFoundScreen String
    | CodeTypingScreen Code.Model
    | MachineMissingDependenciesScreen Int
    | CodeMissingDependenciesScreen


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


type alias ScenarioElementDependency =
    { from : Int
    , to : Int
    }


type DependenciesState
    = AtLeastOneRemaining
    | AllDone


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


type alias ScenarioData =
    { name : String
    , scenarioElements : ScenarioElements
    , timer : Timer
    , penaltyOnFailedAttempt : Int -> AttemptPenalty
    }


type alias Seconds =
    Int


type Reward
    = Modifier Int
    | Time Seconds


fromScenarioData : ScenarioData -> Model
fromScenarioData scenarioData =
    Model NotStarted 0 MainScreen scenarioData.name scenarioData.scenarioElements scenarioData.timer scenarioData.penaltyOnFailedAttempt


update : Msg -> Model -> Model
update msg model =
    case ( model.state, msg ) of
        ( Running, TimerMsg message ) ->
            let
                ( newTimer, timerStatus ) =
                    Timer.update message model.timer
            in
            if timerStatus.remaining <= 0 then
                { model | state = Lost, timer = newTimer }

            else
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
                        |> Maybe.andThen (getMachineFromId model.scenarioElements)
            in
            case machineMaybe of
                Just ( machineId, machine, dependenciesState ) ->
                    if dependenciesState == AtLeastOneRemaining then
                        { model | screen = MachineMissingDependenciesScreen machineId }

                    else
                        { model | screen = MachineScreen machineId machine }

                Nothing ->
                    { model | screen = MachineNotFoundScreen machineIdString }

        ( Running, OnMachineIdInput machineIdString ) ->
            case model.screen of
                MachineSelectionScreen _ ->
                    { model | screen = MachineSelectionScreen machineIdString }

                _ ->
                    model

        _ ->
            model


start : Model -> Model
start model =
    let
        startedTimer =
            Timer.start model.timer
    in
    { model | state = Running, timer = startedTimer }


resume : Model -> Model
resume model =
    let
        startedTimer =
            Timer.start model.timer
    in
    if model.state == NotStarted || model.state == Paused then
        { model | state = Running, timer = startedTimer }

    else
        model


updateMachine : Int -> Machine.Model -> Machine.Msg -> Model -> Model
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


updateCode : Code.Model -> Code.Msg -> Model -> Model
updateCode codeModel codeMsg model =
    let
        ( newCodeModel, codeMaybe ) =
            Code.update codeMsg codeModel

        validCodeIdMaybe =
            codeMaybe |> Maybe.andThen (findCodeIdInElements model.scenarioElements)
    in
    case ( codeMaybe, validCodeIdMaybe ) of
        ( Just _, Just ( codeId, dependenciesState ) ) ->
            let
                updatedScenarioElements =
                    Graph.update codeId flagNodeContextAsDone model.scenarioElements

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
            case dependenciesState of
                AtLeastOneRemaining ->
                    { model | screen = CodeMissingDependenciesScreen }

                AllDone ->
                    { model
                        | screen = MainScreen
                        , scenarioElements = updatedScenarioElements
                        , state = scenarioState
                    }

        ( Just _, Nothing ) ->
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


view : Model -> Html Msg
view model =
    case model.state of
        NotStarted ->
            div [] [ text "Le scénario n'a pas encore démarré..." ]

        Running ->
            viewScreen model

        Paused ->
            div []
                [ p [] [ text "Le jeu est en pause, n'en profitez pas pour tricher ! ;-)" ]
                , div [] [ button [ classes defaultButtonClasses, onClick Resume ] [ FeatherIcons.play |> FeatherIcons.toHtml [] ] ]
                ]

        Won ->
            div [ classes [ f2 ] ] [ text "Vous avez gagné, félicitations !" ]

        Lost ->
            div [ classes [ f2 ] ] [ text "Trop tard, vous avez perdu..." ]


viewScreen : Model -> Html Msg
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

            MachineMissingDependenciesScreen machineId ->
                viewMachineMissingDependenciesScreen machineId

            CodeMissingDependenciesScreen ->
                viewCodeMissingDependenciesScreen
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
                [ legend [ classes [ pa0, f5, f4_ns, mb3, white, tl ] ] [ text "Saisissez le code correspondant à la machine" ]
                , div
                    [ classes [ cf ] ]
                    [ label [ classes [ clip ], for "machineId" ] [ text "Code de la machine" ]
                    , input [ autocomplete False, autofocus True, classes [ border_box, bn, f6, f5_l, input_reset, fl, black_80, bg_white, pa3, lh_solid, w_100, w_75_m, w_80_l, br2_ns, br__left_ns ], id "machineId", value machineId, onInput OnMachineIdInput ] []
                    , input [ type_ "submit", classes [ f6, f5_l, button_reset, fl, pv3, tc, bn, bg_animate, bg_black_70, hover_bg_black, white, pointer, w_100, w_25_m, w_20_l, br2_ns, br__right_ns ] ] [ text "Montrer la machine" ]
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
        [ "La machine " ++ machineIdString ++ " n'a pas été trouvée, êtes-vous sûr d'avoir entré le bon identifiant ?" |> text
        , backToMainScreenButton
        ]


viewMachineMissingDependenciesScreen : Int -> Html Msg
viewMachineMissingDependenciesScreen machineId =
    div []
        [ p [ classes [ mb3 ] ] [ "Vous ne pouvez pas encore utiliser la machine " ++ String.fromInt machineId ++ ", peut-être avez-vous d'autres choses à faire avant..." |> text ]
        , backToMainScreenButton
        ]


viewCodeMissingDependenciesScreen : Html Msg
viewCodeMissingDependenciesScreen =
    div []
        [ p [ classes [ mb3 ] ] [ "Ce code semble correct, mais vous ne pouvez pas encore l'utiliser ; vous avez peut-être d'autres choses à faire avant..." |> text ]
        , backToMainScreenButton
        ]


viewElementDoneScreen : Int -> ScenarioElement -> Html Msg
viewElementDoneScreen _ scenarioElement =
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
            div [ classes [ w3, h3, bg_blue, white, flex, items_center, justify_center, f3, center ] ] [ "+" ++ String.fromInt value |> text ]

        Time time ->
            div [ classes [ w4, h3, bg_green, white, flex, items_center, justify_center, f3, center ] ] [ "+" ++ String.fromInt time ++ "s" |> text ]



-- TODO localeTimeZone


backToMainScreenButton : Html Msg
backToMainScreenButton =
    div []
        [ button [ classes defaultButtonClasses, onClick BackToMainScreen ] [ FeatherIcons.x |> FeatherIcons.toHtml [] ]
        ]


findCodeIdInElements : ScenarioElements -> String -> Maybe ( Int, DependenciesState )
findCodeIdInElements scenarioElements code =
    let
        nodeMaybe =
            Graph.nodes scenarioElements
                |> List.filter (.label >> .element >> (==) (Code code))
                |> List.head
    in
    nodeMaybe
        |> Maybe.map (\node -> ( node.id, getDependenciesState scenarioElements node.id |> Maybe.withDefault AllDone ))


getMachineFromId : ScenarioElements -> Int -> Maybe ( Int, Machine.Model, DependenciesState )
getMachineFromId scenarioElements machineId =
    case Graph.get machineId scenarioElements of
        Just nodeContext ->
            let
                dependenciesState =
                    getNodeContextDependenciesState scenarioElements nodeContext
            in
            nodeContext.node.label
                |> extractMachine
                |> Maybe.map (\machine -> ( machineId, machine, dependenciesState ))

        Nothing ->
            Nothing


getDependenciesState : ScenarioElements -> Int -> Maybe DependenciesState
getDependenciesState scenarioElements elementId =
    Graph.get elementId scenarioElements
        |> Maybe.map (getNodeContextDependenciesState scenarioElements)


getNodeContextDependenciesState : ScenarioElements -> NodeContext ScenarioElement () -> DependenciesState
getNodeContextDependenciesState scenarioElements nodeContext =
    let
        hasAnyDependencyNotDone =
            IntDict.toList nodeContext.incoming
                |> List.map Tuple.first
                |> List.map (getElementState scenarioElements >> Maybe.withDefault Done)
                |> List.any (\state -> state /= Done)
    in
    if hasAnyDependencyNotDone then
        AtLeastOneRemaining

    else
        AllDone


getElementState : ScenarioElements -> Int -> Maybe ElementState
getElementState scenarioElements elementId =
    Graph.get elementId scenarioElements
        |> Maybe.map (.node >> .label >> .state)


extractMachine : ScenarioElement -> Maybe Machine.Model
extractMachine scenarioElement =
    case scenarioElement.element of
        Machine machine ->
            Just machine

        _ ->
            Nothing


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.state == Running then
        Timer.subscriptions model.timer
            |> Sub.map TimerMsg

    else
        Sub.none


decoder : Decoder ScenarioData
decoder =
    Decode.when (Decode.field "version" Decode.int) ((==) 1) scenarioV1Decoder


scenarioV1Decoder : Decoder ScenarioData
scenarioV1Decoder =
    Decode.map4 ScenarioData
        (Decode.field "name" Decode.string)
        (Decode.field "elements" scenarioElementsDecoder)
        (Decode.field "timer" Timer.decoder)
        (Decode.field "penaltyStrategy" penaltyStrategyDecoder)


scenarioElementsDecoder : Decoder ScenarioElements
scenarioElementsDecoder =
    let
        stringKeysToNumbersDecoder : List ( String, Decode.Value ) -> Decoder (List ( Int, Decode.Value ))
        stringKeysToNumbersDecoder keyValuePairs =
            List.map
                (\( numberString, elementValue ) ->
                    String.toInt numberString
                        |> Maybe.map Decode.succeed
                        |> Maybe.withDefault (Decode.fail "One of the keys is invalid")
                        |> Decode.map (\elementNumber -> ( elementNumber, elementValue ))
                )
                keyValuePairs
                |> Decode.combine

        elementValueToScenarioElementDecoder : Int -> Decoder ( Int, ScenarioElement, List ScenarioElementDependency )
        elementValueToScenarioElementDecoder elementNumber =
            Decode.field "dependsOn"
                (Decode.list
                    (Decode.int
                        |> Decode.map (\dependency -> ScenarioElementDependency dependency elementNumber)
                    )
                )
                |> Decode.map3 (\a b c -> ( a, b, c ))
                    (Decode.succeed elementNumber)
                    scenarioElementDecoder

        numberValuePairsToScenarioElementDecoder : List ( Int, Decode.Value ) -> Decoder (List ( Int, ScenarioElement, List ScenarioElementDependency ))
        numberValuePairsToScenarioElementDecoder numberValuePairs =
            List.map
                (\( elementNumber, elementValue ) ->
                    Decode.decodeValue (elementValueToScenarioElementDecoder elementNumber) elementValue
                        |> Result.mapError Decode.errorToString
                        |> Decode.fromResult
                )
                numberValuePairs
                |> Decode.combine
    in
    Decode.keyValuePairs Decode.value
        |> Decode.andThen stringKeysToNumbersDecoder
        |> Decode.andThen numberValuePairsToScenarioElementDecoder
        |> Decode.andThen scenarioElementsDataToScenarioElementsDecoder


scenarioElementsDataToScenarioElementsDecoder : List ( Int, ScenarioElement, List ScenarioElementDependency ) -> Decoder ScenarioElements
scenarioElementsDataToScenarioElementsDecoder scenarioElementsData =
    let
        existingIds =
            List.map (\( id, _, _ ) -> id) scenarioElementsData

        dependencies =
            List.concatMap (\( _, _, elementDependencies ) -> elementDependencies) scenarioElementsData

        hasUnknownDependencies =
            List.map .to dependencies
                |> List.any (\idToCheck -> List.notMember idToCheck existingIds)

        nodes =
            List.map (\( id, scenarioElement, _ ) -> Node id scenarioElement) scenarioElementsData

        edges =
            List.map (\{ from, to } -> Edge from to ()) dependencies

        scenarioElementsGraph =
            Graph.fromNodesAndEdges nodes edges
    in
    if hasUnknownDependencies then
        Decode.fail "Unknown dependencies have been found"

    else
        scenarioElementsGraph
            |> Graph.checkAcyclic
            |> Result.map (always scenarioElementsGraph)
            |> Result.mapError (\edge -> "There is a cyclic dependency between the following elements of the scenario: " ++ String.fromInt edge.from ++ String.fromInt edge.to)
            |> Decode.fromResult


scenarioElementDecoder : Decoder ScenarioElement
scenarioElementDecoder =
    Decode.oneOf
        [ Decode.when (Decode.field "type" Decode.string) ((==) "code") codeElementDecoder
        , Decode.when (Decode.field "type" Decode.string) ((==) "machine") machineElementDecoder
        ]
        |> Decode.map4 (ScenarioElement ToDo)
            (Decode.field "triggerScenarioSuccess" Decode.bool
                |> Decode.map
                    (\isFinal ->
                        if isFinal then
                            Final

                        else
                            NotFinal
                    )
            )
            (Decode.field "rewards" (Decode.list rewardDecoder))
            (Decode.field "successMessage" Decode.string)


rewardDecoder : Decoder Reward
rewardDecoder =
    Decode.oneOf
        [ Decode.when (Decode.field "type" Decode.string) ((==) "modifier") modifierRewardDecoder
        , Decode.when (Decode.field "type" Decode.string) ((==) "time") timeRewardDecoder
        ]


modifierRewardDecoder : Decoder Reward
modifierRewardDecoder =
    Decode.map Modifier (Decode.at [ "data", "value" ] Decode.int)


timeRewardDecoder : Decoder Reward
timeRewardDecoder =
    Decode.map Time (Decode.at [ "data", "seconds" ] Decode.int)


codeElementDecoder : Decoder Element
codeElementDecoder =
    Decode.at [ "data", "value" ] Decode.string
        |> Decode.map Code


machineElementDecoder : Decoder Element
machineElementDecoder =
    Decode.oneOf
        [ Decode.when (Decode.field "type" Decode.string) ((==) "electricalPanel") electricalPanelDecoder
        , Decode.when (Decode.field "type" Decode.string) ((==) "waterPipe") waterPipeDecoder
        ]
        |> Decode.field "data"


waterPipeDecoder : Decoder Element
waterPipeDecoder =
    Decode.map2 (\legend pipes -> waterPipePuzzle legend pipes)
        (Decode.field "legend" Decode.string)
        (Decode.field "pipes" pipesDecoder)
        |> Decode.map (Result.fromMaybe "Invalid pipes configuration")
        |> Decode.andThen Decode.fromResult
        |> Decode.map Machine


pipesDecoder : Decoder (List (List PipeWithOptions))
pipesDecoder =
    Decode.list (Decode.list pipeDecoder)


pipeDecoder : Decoder PipeWithOptions
pipeDecoder =
    Decode.map2 Tuple.pair
        (Decode.oneOf
            [ Decode.when (Decode.field "type" Decode.string) ((==) "opening") openingPipeDecoder
            , normalPipeDecoder
            ]
        )
        (Decode.field "options" (Decode.list pipeOptionsDecoder))


openingPipeDecoder : Decoder Pipe
openingPipeDecoder =
    Decode.map2 Opening
        (Decode.field "direction" pipeDirectionDecoder)
        (Decode.field "openingType" openingTypeDecoder)


pipeDirectionDecoder : Decoder Direction
pipeDirectionDecoder =
    Decode.string
        |> Decode.andThen
            (\directionString ->
                case directionString of
                    "top" ->
                        Decode.succeed Top

                    "right" ->
                        Decode.succeed Right

                    "bottom" ->
                        Decode.succeed Bottom

                    "left" ->
                        Decode.succeed Left

                    unknown ->
                        Decode.fail ("Unknown direction for opening pipe: \"" ++ unknown ++ "\"")
            )


pipeOptionsDecoder : Decoder Option
pipeOptionsDecoder =
    Decode.string
        |> Decode.andThen
            (\optionString ->
                case optionString of
                    "notRotatable" ->
                        Decode.succeed NotRotatable

                    unknown ->
                        Decode.fail ("Unknown option for pipe: \"" ++ unknown ++ "\"")
            )


normalPipeDecoder : Decoder Pipe
normalPipeDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\pipeType ->
                case pipeType of
                    "connector4" ->
                        Decode.succeed Connector4

                    "bottomLeftConnector" ->
                        Decode.succeed BottomLeftConnector

                    "bottomRightConnector" ->
                        Decode.succeed BottomRightConnector

                    "bottomT" ->
                        Decode.succeed BottomT

                    "leftRightConnector" ->
                        Decode.succeed LeftRightConnector

                    "leftT" ->
                        Decode.succeed LeftT

                    "rightT" ->
                        Decode.succeed RightT

                    "topLeftConnector" ->
                        Decode.succeed TopLeftConnector

                    "topBottomConnector" ->
                        Decode.succeed TopBottomConnector

                    "topRightConnector" ->
                        Decode.succeed TopRightConnector

                    "topT" ->
                        Decode.succeed TopT

                    unknown ->
                        Decode.fail ("Unknown pipe type: \"" ++ unknown ++ "\"")
            )


openingTypeDecoder : Decoder OpeningType
openingTypeDecoder =
    Decode.string
        |> Decode.andThen
            (\openingTypeString ->
                case openingTypeString of
                    "entrance" ->
                        Decode.succeed Entrance

                    "exit" ->
                        Decode.succeed Exit

                    unknown ->
                        Decode.fail ("Unknown opening type for opening pipe: \"" ++ unknown ++ "\"")
            )


electricalPanelDecoder : Decoder Element
electricalPanelDecoder =
    Decode.map2 (\legend expectedPlugs -> simpleElectricalPanel legend expectedPlugs)
        (Decode.field "legend" Decode.string)
        (Decode.field "expectedPlugs" (Decode.list plugTupleDecoder))
        |> Decode.map Machine


plugTupleDecoder : Decoder ( Plug, Plug )
plugTupleDecoder =
    Decode.map2 Tuple.pair
        (Decode.field "from" plugDecoder)
        (Decode.field "to" plugDecoder)


plugDecoder : Decoder Plug
plugDecoder =
    Decode.string
        |> Decode.andThen
            (\plugString ->
                case plugString of
                    "top left" ->
                        Decode.succeed TopLeft

                    "top center" ->
                        Decode.succeed TopCenter

                    "top right" ->
                        Decode.succeed TopRight

                    "bottom left" ->
                        Decode.succeed BottomLeft

                    "bottom center" ->
                        Decode.succeed BottomCenter

                    "bottom right" ->
                        Decode.succeed BottomRight

                    unknown ->
                        "Unknown pipe name received: \"" ++ unknown ++ "\"" |> Decode.fail
            )


penaltyStrategyDecoder : Decoder (Int -> AttemptPenalty)
penaltyStrategyDecoder =
    Decode.oneOf
        [ Decode.when (Decode.field "type" Decode.string) ((==) "simpleTimeRemoval") simpleTimeRemovalStrategyDecoder
        ]


simpleTimeRemovalStrategyDecoder : Decoder (Int -> AttemptPenalty)
simpleTimeRemovalStrategyDecoder =
    Decode.map3
        (\threshold beforeThresholdInSeconds afterOrEqualToThresholdInSeconds failedAttempts ->
            (if failedAttempts < threshold then
                beforeThresholdInSeconds

             else
                afterOrEqualToThresholdInSeconds
            )
                * 1000
                |> RemoveTime
        )
        (Decode.field "threshold" Decode.int)
        (Decode.field "beforeThresholdInSeconds" Decode.int)
        (Decode.field "afterOrEqualToThresholdInSeconds" Decode.int)
