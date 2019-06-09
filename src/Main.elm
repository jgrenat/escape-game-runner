module Main exposing (main)

import Browser exposing (Document)
import Html exposing (Html, button, div, h1, p, text)
import Html.Events exposing (onClick)
import Json.Decode as Decode exposing (Value)
import Scenario.Scenario as Scenario exposing (State(..))
import Styles.Styles exposing (validateButtonClasses)
import Tachyons exposing (classes, tachyons)
import Tachyons.Classes exposing (center, f5, f_subheadline, lh_solid, mw6, ph2, sans_serif)



---- MODEL ----


type Model
    = ValidScenario Scenario.Model
    | InvalidScenario String


init : Value -> ( Model, Cmd Msg )
init _ =
    (case Decode.decodeString Scenario.decoder tutorialScenario of
        Ok scenario ->
            ValidScenario (Scenario.fromScenarioData scenario)

        Err error ->
            InvalidScenario (Decode.errorToString error)
    )
        |> (\scenario -> ( scenario, Cmd.none ))



---- UPDATE ----


type Msg
    = StartScenario
    | ScenarioMsg Scenario.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        ValidScenario scenario ->
            case msg of
                StartScenario ->
                    ( ValidScenario (Scenario.start scenario), Cmd.none )

                ScenarioMsg scenarioMsg ->
                    let
                        newScenario =
                            Scenario.update scenarioMsg scenario
                    in
                    ( ValidScenario newScenario, Cmd.none )

        InvalidScenario _ ->
            ( model, Cmd.none )



---- VIEW ----


view : Model -> Document Msg
view model =
    case model of
        ValidScenario scenario ->
            Document scenario.name
                [ div [ classes [ mw6, center, sans_serif, f5, ph2 ] ]
                    [ tachyons.css
                    , h1 [ classes [ f_subheadline, lh_solid ] ] [ text "Unlock" ]
                    , if scenario.state == NotStarted then
                        notStartedView

                      else
                        Scenario.view scenario |> Html.map ScenarioMsg
                    ]
                ]

        InvalidScenario _ ->
            Document "Invalid scenario" [ text "Oh, it looks like this scenario is invalid !?" ]


tutorialScenario : String
tutorialScenario =
    """
{
  "version": 1,
  "name": "Tutorial",
  "timer": {
    "initialTimeInSeconds": 600
  },
  "penaltyStrategy": {
    "type": "simpleTimeRemoval",
    "threshold": 5,
    "beforeThresholdInSeconds": 60,
    "afterOrEqualToThresholdInSeconds": 30
  },
  "elements": {
    "69": {
      "type": "machine",
      "data": {
        "type": "electricalPanel",
        "legend": "Pour utiliser cette machine vous devez appuyer sur les boutons correspondant aux bons picots et appuyer sur Ok. Chaque erreur vous fait perdre une minute, il est donc préférable d'attendre de comprendre quels sont les bon picots avant de les utiliser.",
        "expectedPlugs": [
          {
            "from": "top center",
            "to": "bottom center"
          }
        ]
      },
      "successMessage": "Bravo, additionnez ce numéro avec un autre numéro !",
      "rewards": [
        {
          "type": "modifier",
          "data": {
            "value": 9
          }
        }
      ],
      "triggerScenarioSuccess": false,
      "dependsOn": []
    },
    "48": {
      "type": "code",
      "data": {
        "value": "9372"
      },
      "successMessage": "Bravo, vous avez gagné !",
      "rewards": [],
      "triggerScenarioSuccess": true,
      "dependsOn": [
        69
      ]
    },
    "1": {
      "type": "machine",
      "data": {
        "type": "waterPipe",
        "legend": "water pipe puzzle",
        "pipes": [
          [
            {
              "type": "opening",
              "direction": "right",
              "openingType": "entrance",
              "options": [
                "notRotatable"
              ]
            },
            {
              "type": "leftT",
              "options": []
            },
            {
              "type": "topRightConnector",
              "options": []
            },
            {
              "type": "leftRightConnector",
              "options": []
            }
          ],
          [
            {
              "type": "bottomRightConnector",
              "options": []
            },
            {
              "type": "topBottomConnector",
              "options": []
            },
            {
              "type": "leftRightConnector",
              "options": []
            },
            {
              "type": "leftRightConnector",
              "options": []
            }
          ],
          [
            {
              "type": "bottomRightConnector",
              "options": []
            },
            {
              "type": "topT",
              "options": []
            },
            {
              "type": "leftT",
              "options": []
            },
            {
              "type": "leftRightConnector",
              "options": []
            }
          ],
          [
            {
              "type": "bottomRightConnector",
              "options": []
            },
            {
              "type": "topRightConnector",
              "options": []
            },
            {
              "type": "leftT",
              "options": []
            },
            {
              "type": "opening",
              "direction": "left",
              "openingType": "exit",
              "options": []
            }
          ]
        ]
      },
      "successMessage": "Bravo, additionnez ce numéro avec un autre numéro !",
      "rewards": [
        {
          "type": "modifier",
          "data": {
            "value": 9
          }
        }
      ],
      "triggerScenarioSuccess": false,
      "dependsOn": []
    }
  }
}"""


notStartedView : Html Msg
notStartedView =
    p []
        [ button [ onClick StartScenario, classes validateButtonClasses ] [ text "Start scenario" ]
        ]



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        ValidScenario scenario ->
            Scenario.subscriptions scenario
                |> Sub.map ScenarioMsg

        InvalidScenario _ ->
            Sub.none



---- PROGRAM ----


main : Program Value Model Msg
main =
    Browser.document
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
