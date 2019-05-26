module Scenario.Tutorial exposing (Msg, scenarioData)

import Attempt exposing (AttemptPenalty(..))
import Code.Code as Code
import Graph exposing (Edge, Graph, Node)
import Machine.Machine as Machine
import Machine.SimpleElectricalPanel exposing (Plug(..))
import Machine.WaterPipePuzzle exposing (Direction(..), OpeningType(..), Option(..), Pipe(..))
import Scenario.Scenario exposing (Element(..), ElementState(..), FinalState(..), Reward(..), ScenarioData, ScenarioElement)
import Timer.Timer as Timer exposing (Timer)


type Msg
    = CodeMessage Code.Msg


machine : ScenarioElement
machine =
    Machine.simpleElectricalPanel "Pour utiliser cette machine vous devez appuyer sur les boutons correspondant aux bons picots et appuyer sur Ok. Chaque erreur vous fait perdre une minute, il est donc préférable d'attendre de comprendre quels sont les bon picots avant de les utiliser." [ ( TopCenter, BottomCenter ) ]
        |> Machine
        |> ScenarioElement ToDo NotFinal [ Modifier 9 ] "Bravo, additionnez ce numéro avec un autre numéro !"


waterPipeMachine : Maybe ScenarioElement
waterPipeMachine =
    [ [ ( Opening Right Entrance, [ NotRotatable ] ), ( LeftT, [] ), ( TopRightConnector, [] ), ( LeftRightConnector, [] ) ]
    , [ ( BottomRightConnector, [] ), ( TopBottomConnector, [] ), ( LeftRightConnector, [] ), ( LeftRightConnector, [] ) ]
    , [ ( BottomRightConnector, [] ), ( TopT, [] ), ( LeftT, [] ), ( LeftRightConnector, [] ) ]
    , [ ( BottomRightConnector, [] ), ( TopRightConnector, [] ), ( LeftT, [] ), ( Opening Left Exit, [] ) ]
    ]
        |> Machine.waterPipePuzzle "water pipe puzzle"
        |> Maybe.map Machine
        |> Maybe.map (ScenarioElement ToDo NotFinal [ Modifier 9 ] "Bravo, additionnez ce numéro avec un autre numéro !")


code : ScenarioElement
code =
    "9372"
        |> Code
        |> ScenarioElement ToDo Final [] "Bravo, vous avez gagné !"


scenarioElementsNodes : List (Node ScenarioElement)
scenarioElementsNodes =
    [ Node 69 machine |> Just, Node 48 code |> Just, waterPipeMachine |> Maybe.map (Node 1) ]
        |> List.filterMap identity


scenarioElementsEdges : List (Edge ())
scenarioElementsEdges =
    [ Edge 69 48 () ]


scenarioElements : Graph ScenarioElement ()
scenarioElements =
    Graph.fromNodesAndEdges scenarioElementsNodes scenarioElementsEdges


scenarioTimer : Timer
scenarioTimer =
    Timer.init (10 * 60 * 1000)


penaltyOnFailedAttempt : Int -> AttemptPenalty
penaltyOnFailedAttempt failedAttempts =
    if failedAttempts < 5 then
        RemoveTime (60 * 1000)

    else
        30 * 1000 |> RemoveTime


scenarioData : ScenarioData
scenarioData =
    ScenarioData "Tutorial" scenarioElements scenarioTimer penaltyOnFailedAttempt
