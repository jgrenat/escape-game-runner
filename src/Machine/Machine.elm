module Machine.Machine exposing (Model, Msg, simpleElectricalPanel, view, update)

import Html exposing (Html)
import Machine.SimpleElectricalPanel as SimpleElectricalPanel exposing (Plug)
import Attempt exposing (AttemptStatus)


type Model
    = SimpleElectricalPanelMachine SimpleElectricalPanel.Model


type Msg
    = MachineMsg
    | SimpleElectricalPanelMsg SimpleElectricalPanel.Msg


update : Msg -> Model -> ( Model, Maybe AttemptStatus )
update msg model =
    case ( model, msg ) of
        ( SimpleElectricalPanelMachine machine, SimpleElectricalPanelMsg message ) ->
            SimpleElectricalPanel.update message machine
                |> Tuple.mapFirst SimpleElectricalPanelMachine

        _ ->
            ( model, Nothing )


simpleElectricalPanel : String -> List ( Plug, Plug ) -> Model
simpleElectricalPanel text expectedPlugs =
    SimpleElectricalPanel.init text expectedPlugs
        |> SimpleElectricalPanelMachine


view : Int -> Model -> Html Msg
view machineId machine =
    case machine of
        SimpleElectricalPanelMachine simpleElectricalPanel ->
            SimpleElectricalPanel.view simpleElectricalPanel
                |> Html.map SimpleElectricalPanelMsg
