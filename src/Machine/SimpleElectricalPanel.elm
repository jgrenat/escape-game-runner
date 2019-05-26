module Machine.SimpleElectricalPanel exposing (Model, Msg, Plug(..), init, update, view)

import Attempt exposing (AttemptStatus(..))
import Html exposing (Html, button, div, p, text)
import Styles.Styles exposing (defaultButtonClasses, validateButtonClasses)
import Svg exposing (Svg, circle, line, rect, svg)
import Svg.Attributes exposing (class, cx, cy, fill, height, r, rx, ry, stroke, strokeWidth, viewBox, width, x, x1, x2, y, y1, y2)
import Svg.Events exposing (onClick)
import Tachyons exposing (classes)
import Tachyons.Classes exposing (pointer, w_100)


type alias Model =
    { selectedPlug : Maybe Plug
    , currentLinks : List ( Plug, Plug )
    , expectedLinks : List ( Plug, Plug )
    , textToDisplay : String
    }


type Plug
    = TopLeft
    | TopCenter
    | TopRight
    | BottomLeft
    | BottomCenter
    | BottomRight


type Msg
    = Reset
    | OnPlugClicked Plug
    | Attempt


init : String -> List ( Plug, Plug ) -> Model
init textToDisplay expectedLinks =
    Model Nothing [] expectedLinks textToDisplay


update : Msg -> Model -> ( Model, Maybe AttemptStatus )
update msg model =
    case msg of
        Reset ->
            ( { model | currentLinks = [] }, Nothing )

        OnPlugClicked plug ->
            case model.selectedPlug of
                Nothing ->
                    ( { model | selectedPlug = Just plug }, Nothing )

                Just selectedPlug ->
                    if model.selectedPlug == Just plug then
                        ( { model | selectedPlug = Nothing }, Nothing )

                    else
                        let
                            newLinks =
                                model.currentLinks
                                    |> List.filter ((/=) ( plug, selectedPlug ))
                                    |> List.filter ((/=) ( selectedPlug, plug ))
                                    |> List.append [ ( selectedPlug, plug ) ]
                        in
                        ( { model | selectedPlug = Nothing, currentLinks = newLinks }, Nothing )

        Attempt ->
            let
                isValid =
                    areLinksValid model.expectedLinks model.currentLinks

                attemptStatus =
                    if isValid then
                        Correct

                    else
                        Incorrect
            in
            ( model, Just attemptStatus )


areLinksValid : List ( Plug, Plug ) -> List ( Plug, Plug ) -> Bool
areLinksValid expectedLinks links =
    let
        notContainedLinks =
            List.filter (doesNotContainLink links) expectedLinks

        extraLinks =
            List.filter (doesNotContainLink expectedLinks) links
    in
    List.isEmpty notContainedLinks && List.isEmpty extraLinks


doesNotContainLink : List ( Plug, Plug ) -> ( Plug, Plug ) -> Bool
doesNotContainLink list link =
    List.any (\( plug1, plug2 ) -> ( plug1, plug2 ) == link || ( plug2, plug1 ) == link) list
        |> not


view : Model -> Html Msg
view model =
    let
        plugs =
            viewPlugs model.selectedPlug

        links =
            viewLinks model.currentLinks

        machineBackground =
            rect [ x "5%", y "5%", width "90%", height "90%", rx "15", ry "15" ] []
    in
    div []
        [ svg
            [ width "400", height "300", viewBox "0 0 300 250", fill "white", stroke "black", strokeWidth "3", class w_100 ]
            (machineBackground :: List.append plugs links)
        , p [] [ text model.textToDisplay ]
        , viewControls
        ]


viewLinks : List ( Plug, Plug ) -> List (Svg Msg)
viewLinks plugs =
    plugs
        |> List.map viewLink


viewLink : ( Plug, Plug ) -> Svg Msg
viewLink ( plug1, plug2 ) =
    let
        ( px1, py1 ) =
            toStringCoordinates plug1

        ( px2, py2 ) =
            toStringCoordinates plug2
    in
    line [ x1 px1, y1 py1, x2 px2, y2 py2, stroke "red", strokeWidth "5px" ] []


viewPlugs : Maybe Plug -> List (Svg Msg)
viewPlugs selectedPlug =
    [ TopLeft, TopCenter, TopRight, BottomLeft, BottomCenter, BottomRight ]
        |> List.map (viewPlug selectedPlug)


viewPlug : Maybe Plug -> Plug -> Svg Msg
viewPlug selectedPlug plug =
    let
        ( x, y ) =
            getPlugCoordinates plug

        isSelected =
            Just plug == selectedPlug

        fillColor =
            if isSelected then
                "yellow"

            else
                "transparent"
    in
    circle [ cx (String.fromInt x ++ "%"), cy (String.fromInt y ++ "%"), r "15", fill fillColor, onClick (OnPlugClicked plug), class pointer ] []


viewControls : Html Msg
viewControls =
    div []
        [ button [ classes defaultButtonClasses, onClick Reset ] [ text "Reset" ]
        , button [ classes validateButtonClasses, onClick Attempt ] [ text "Try" ]
        ]


getPlugCoordinates : Plug -> ( Int, Int )
getPlugCoordinates plug =
    case plug of
        TopLeft ->
            ( 25, 25 )

        TopCenter ->
            ( 50, 25 )

        TopRight ->
            ( 75, 25 )

        BottomLeft ->
            ( 25, 75 )

        BottomCenter ->
            ( 50, 75 )

        BottomRight ->
            ( 75, 75 )


toStringCoordinates : Plug -> ( String, String )
toStringCoordinates plug =
    let
        ( x, y ) =
            getPlugCoordinates plug
    in
    ( String.fromInt x ++ "%", String.fromInt y ++ "%" )
