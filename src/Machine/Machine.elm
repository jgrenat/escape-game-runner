module Machine.Machine exposing (Model, Msg, simpleElectricalPanel, waterPipePuzzle, view, update)

import Html exposing (Html)
import Machine.SimpleElectricalPanel as SimpleElectricalPanel exposing (Plug)
import Machine.WaterPipePuzzle as WaterPipePuzzle exposing (Pipe, PipeWithOptions)
import Attempt exposing (AttemptStatus)


type Model
    = SimpleElectricalPanelMachine SimpleElectricalPanel.Model
    | WaterPipePuzzleMachine WaterPipePuzzle.Model


type Msg
    = SimpleElectricalPanelMsg SimpleElectricalPanel.Msg
    | WaterPipePuzzleMsg WaterPipePuzzle.Msg


update : Msg -> Model -> ( Model, Maybe AttemptStatus )
update msg model =
    case ( model, msg ) of
        ( SimpleElectricalPanelMachine machine, SimpleElectricalPanelMsg message ) ->
            SimpleElectricalPanel.update message machine
                |> Tuple.mapFirst SimpleElectricalPanelMachine

        ( WaterPipePuzzleMachine machine, WaterPipePuzzleMsg message ) ->
            WaterPipePuzzle.update message machine
                |> Tuple.mapFirst WaterPipePuzzleMachine

        _ ->
            ( model, Nothing )


simpleElectricalPanel : String -> List ( Plug, Plug ) -> Model
simpleElectricalPanel text expectedPlugs =
    SimpleElectricalPanel.init text expectedPlugs
        |> SimpleElectricalPanelMachine


waterPipePuzzle : String -> List (List PipeWithOptions) -> Model
waterPipePuzzle text pipes =
    WaterPipePuzzle.init pipes
        |> WaterPipePuzzleMachine


view : Int -> Model -> Html Msg
view machineId machine =
    case machine of
        SimpleElectricalPanelMachine simpleElectricalPanel ->
            SimpleElectricalPanel.view simpleElectricalPanel
                |> Html.map SimpleElectricalPanelMsg

        WaterPipePuzzleMachine waterPipePuzzle ->
            WaterPipePuzzle.view waterPipePuzzle
                |> Html.map WaterPipePuzzleMsg
