module Scenario.Tutorial exposing (Msg, scenarioData)

import Attempt exposing (AttemptPenalty(RemoveTime))
import Code.Code as Code
import Graph exposing (Edge, Graph, Node)
import Machine.Machine as Machine
import Machine.SimpleElectricalPanel exposing (Plug(BottomCenter, TopCenter))
import Scenario.Scenario exposing (Element(Code, Machine), ElementState(ToDo), FinalState(Final, NotFinal), Reward(Modifier, Time), ScenarioData, ScenarioElement)
import Time exposing (minute, second)
import Timer.Timer as Timer exposing (Timer)


type Msg
    = CodeMessage Code.Msg


machine : ScenarioElement
machine =
    Machine.simpleElectricalPanel "Pour utiliser cette machine vous devez appuyer sur les boutons correspondant aux bons picots et appuyer sur Ok. Chaque erreur vous fait perdre une minute, il est donc préférable d'attendre de comprendre quels sont les bon picots avant de les utiliser." [ ( TopCenter, BottomCenter ) ]
        |> Machine
        |> ScenarioElement ToDo NotFinal [ Modifier 9 ] "Bravo, additionnez ce numéro avec un autre numéro !"


code : ScenarioElement
code =
    "9372"
        |> Code
        |> ScenarioElement ToDo Final [] "Congrats, you've won!"


scenarioElementsNodes : List (Node ScenarioElement)
scenarioElementsNodes =
    [ Node 69 machine, Node 48 code ]


scenarioElementsEdges : List (Edge ())
scenarioElementsEdges =
    [ Edge 69 48 () ]


scenarioElements : Graph ScenarioElement ()
scenarioElements =
    Graph.fromNodesAndEdges scenarioElementsNodes scenarioElementsEdges


scenarioTimer : Timer Msg
scenarioTimer =
    Timer.init (10 * minute)


penaltyOnFailedAttempt : Int -> AttemptPenalty
penaltyOnFailedAttempt failedAttempts =
    if failedAttempts < 5 then
        RemoveTime minute
    else
        30 * second |> RemoveTime


scenarioData : ScenarioData Msg
scenarioData =
    ScenarioData "Tutorial" scenarioElements scenarioTimer penaltyOnFailedAttempt
