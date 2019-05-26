module Main exposing (Model, Msg(..), applyUpdate, applyUpdates, init, main, notStartedView, subscriptions, update, view)

import Browser exposing (Document)
import Html exposing (Html, button, div, h1, p, text)
import Html.Events exposing (onClick)
import Json.Decode exposing (Value)
import Scenario.Scenario as Scenario exposing (State(..))
import Scenario.Tutorial exposing (scenarioData)
import Styles.Styles exposing (validateButtonClasses)
import Tachyons exposing (classes, tachyons)
import Tachyons.Classes exposing (center, f5, f_subheadline, lh_solid, mw6, ph2, sans_serif)



---- MODEL ----


type alias Model =
    { scenario : Scenario.Model }


init : Value -> ( Model, Cmd Msg )
init _ =
    ( { scenario = Scenario.fromScenarioData scenarioData }, Cmd.none )



---- UPDATE ----


type Msg
    = StartScenario
    | ScenarioMsg Scenario.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        StartScenario ->
            ( { model | scenario = Scenario.start model.scenario }, Cmd.none )

        ScenarioMsg scenarioMsg ->
            let
                newScenario =
                    Scenario.update scenarioMsg model.scenario
            in
            ( { model | scenario = newScenario }, Cmd.none )



---- VIEW ----


view : Model -> Document Msg
view model =
    Document "title"
        [ div [ classes [ mw6, center, sans_serif, f5, ph2 ] ]
            [ tachyons.css
            , h1 [ classes [ f_subheadline, lh_solid ] ] [ text "Unlock" ]
            , if model.scenario.state == NotStarted then
                notStartedView

              else
                Scenario.view model.scenario |> Html.map ScenarioMsg
            ]
        ]


notStartedView : Html Msg
notStartedView =
    p []
        [ button [ onClick StartScenario, classes validateButtonClasses ] [ text "Start scenario" ]
        ]



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions model =
    Scenario.subscriptions model.scenario
        |> Sub.map ScenarioMsg



---- PROGRAM ----


main : Program Value Model Msg
main =
    Browser.document
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }


applyUpdates : List Msg -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
applyUpdates messages ( model, cmd ) =
    List.foldr applyUpdate ( model, [ cmd ] ) messages
        |> Tuple.mapSecond Cmd.batch


applyUpdate : Msg -> ( Model, List (Cmd Msg) ) -> ( Model, List (Cmd Msg) )
applyUpdate msg ( currentModel, currentCommands ) =
    let
        ( newModel, cmd ) =
            update msg currentModel

        newCommands =
            if cmd == Cmd.none then
                currentCommands

            else
                cmd :: currentCommands
    in
    ( newModel, newCommands )
