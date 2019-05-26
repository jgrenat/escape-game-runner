module Machine.Machine exposing (Model, Msg, simpleElectricalPanel, update, view, waterPipePuzzle)

import Attempt exposing (AttemptStatus)
import Html exposing (Html)
import Machine.SimpleElectricalPanel as SimpleElectricalPanel exposing (Plug)
import Machine.WaterPipePuzzle as WaterPipePuzzle exposing (Pipe, PipeWithOptions)


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


waterPipePuzzle : String -> List (List PipeWithOptions) -> Maybe Model
waterPipePuzzle _ pipes =
    WaterPipePuzzle.init pipes
        |> Maybe.map WaterPipePuzzleMachine


view : Int -> Model -> Html Msg
view _ machine =
    case machine of
        SimpleElectricalPanelMachine simpleElectricalPanelModel ->
            SimpleElectricalPanel.view simpleElectricalPanelModel
                |> Html.map SimpleElectricalPanelMsg

        WaterPipePuzzleMachine waterPipePuzzleModel ->
            WaterPipePuzzle.view waterPipePuzzleModel
                |> Html.map WaterPipePuzzleMsg
