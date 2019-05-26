module Machine.WaterPipePuzzle exposing (Direction(..), GameBoard, Model, Msg, OpeningType(..), Option(..), Pipe(..), PipeWithOptions, init, update, view)

import Array
import Attempt exposing (AttemptStatus(..))
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Matrix exposing (Matrix)
import Styles.Styles exposing (validateButtonClasses)
import Svg exposing (Svg, line, polygon, svg)
import Svg.Attributes exposing (class, fill, points, stroke, strokeWidth, viewBox, x1, x2, y1, y2)
import Tachyons exposing (classes)
import Tachyons.Classes exposing (mt3, pointer)


type Pipe
    = Connector4
    | Opening Direction OpeningType
    | BottomLeftConnector
    | BottomRightConnector
    | BottomT
    | LeftRightConnector
    | LeftT
    | RightT
    | TopLeftConnector
    | TopBottomConnector
    | TopRightConnector
    | TopT


type alias Model =
    { gameBoard : GameBoard
    }


type alias GameBoard =
    Matrix ( Pipe, List Option )


type alias WateredGameBoard =
    Matrix ( Pipe, WaterState )


type alias PipeWithOptions =
    ( Pipe, List Option )


type Option
    = NotRotatable


type OpeningType
    = Entrance
    | Exit


type Direction
    = Top
    | Right
    | Bottom
    | Left


type WaterState
    = Out
    | Flooded
    | NothingSpecial
    | NotVisited


type alias Location =
    { row : Int
    , column : Int
    }


type Msg
    = Rotate Location
    | Attempt


init : List (List ( Pipe, List Option )) -> Maybe Model
init pipesWithOptions =
    case pipesWithOptions of
        [] ->
            Nothing

        first :: _ ->
            let
                pipesArray =
                    List.map Array.fromList pipesWithOptions
                        |> Array.fromList
            in
            Matrix.generate (List.length pipesWithOptions) (List.length first) (\row column -> Array.get column pipesArray |> Maybe.andThen (Array.get row))
                |> Matrix.map (Maybe.withDefault ( Connector4, [] ))
                |> Model
                |> Just


update : Msg -> Model -> ( Model, Maybe AttemptStatus )
update msg model =
    case msg of
        Rotate location ->
            let
                updatedBoard =
                    Matrix.get location.column location.row model.gameBoard
                        |> Result.toMaybe
                        |> Maybe.map rotateRightPipe
                        |> Maybe.map (\rotatedPipe -> Matrix.set location.column location.row rotatedPipe model.gameBoard)
                        |> Maybe.withDefault model.gameBoard
            in
            ( { model | gameBoard = updatedBoard }, Nothing )

        Attempt ->
            let
                waterOut =
                    checkWaterOutAndNotFlooded model.gameBoard
            in
            if waterOut then
                ( model, Just Correct )

            else
                ( model, Just Incorrect )


checkWaterOutAndNotFlooded : GameBoard -> Bool
checkWaterOutAndNotFlooded gameBoard =
    let
        wateredGameBoard : WateredGameBoard
        wateredGameBoard =
            Matrix.map (\( pipe, _ ) -> ( pipe, NotVisited )) gameBoard

        resultingPipesStates =
            getEntrances gameBoard
                |> List.foldl propagateWaterIntoPipe2 wateredGameBoard
                >> Matrix.toArray
                |> Array.toList
                |> List.map Tuple.second
    in
    (not <| List.member Flooded resultingPipesStates)
        && List.member Out resultingPipesStates


getEntrances : GameBoard -> List ( Location, Direction )
getEntrances gameBoard =
    Matrix.indexedMap (\column row ( pipe, _ ) -> ( Location row column, pipe )) gameBoard
        |> Matrix.toArray
        |> Array.toList
        |> List.filterMap
            (\( location, pipe ) ->
                case pipe of
                    Opening openingDirection Entrance ->
                        Just ( location, openingDirection )

                    _ ->
                        Nothing
            )


updateCell : Location -> (a -> a) -> Matrix a -> Matrix a
updateCell location mapFunction matrix =
    Matrix.indexedMap
        (\column row value ->
            if row == location.row && column == location.column then
                mapFunction value

            else
                value
        )
        matrix


propagateWaterIntoPipe2 : ( Location, Direction ) -> WateredGameBoard -> WateredGameBoard
propagateWaterIntoPipe2 ( location, direction ) wateredBoard =
    case Matrix.get location.column location.row wateredBoard of
        Ok ( pipe, state ) ->
            if not <| isAccessibleBy direction pipe then
                let
                    previousLocation =
                        case direction of
                            Top ->
                                toTop location |> Tuple.first

                            Left ->
                                toLeft location |> Tuple.first

                            Bottom ->
                                toBottom location |> Tuple.first

                            Right ->
                                toRight location |> Tuple.first
                in
                updateCell previousLocation (\( currentPipe, _ ) -> ( currentPipe, Flooded )) wateredBoard

            else
                case ( pipe, state ) of
                    ( Opening _ Exit, NotVisited ) ->
                        updateCell location (\( currentPipe, _ ) -> ( currentPipe, Out )) wateredBoard

                    ( currentPipe, NotVisited ) ->
                        let
                            newWateredBoard =
                                updateCell location (\_ -> ( currentPipe, NothingSpecial )) wateredBoard

                            exits : List ( Location, Direction )
                            exits =
                                getPipeExits location currentPipe
                        in
                        List.foldl propagateWaterIntoPipe2 newWateredBoard exits

                    _ ->
                        wateredBoard

        Err _ ->
            let
                previousLocation =
                    case direction of
                        Top ->
                            toTop location |> Tuple.first

                        Left ->
                            toLeft location |> Tuple.first

                        Bottom ->
                            toBottom location |> Tuple.first

                        Right ->
                            toRight location |> Tuple.first
            in
            updateCell previousLocation (\( pipe, _ ) -> ( pipe, Flooded )) wateredBoard


isAccessibleBy : Direction -> Pipe -> Bool
isAccessibleBy direction pipe =
    let
        exits =
            getPipeExits (Location 0 0) pipe |> List.map Tuple.second

        expectedDirection =
            case direction of
                Top ->
                    Bottom

                Right ->
                    Left

                Bottom ->
                    Top

                Left ->
                    Right
    in
    List.member expectedDirection exits


getPipeExits : Location -> Pipe -> List ( Location, Direction )
getPipeExits location pipe =
    case pipe of
        Opening Top _ ->
            [ toTop location ]

        Opening Right _ ->
            [ toRight location ]

        Opening Bottom _ ->
            [ toBottom location ]

        Opening Left _ ->
            [ toLeft location ]

        Connector4 ->
            [ toTop location, toRight location, toBottom location, toLeft location ]

        BottomLeftConnector ->
            [ toBottom location, toLeft location ]

        BottomRightConnector ->
            [ toRight location, toBottom location ]

        BottomT ->
            [ toRight location, toBottom location, toLeft location ]

        LeftRightConnector ->
            [ toRight location, toLeft location ]

        LeftT ->
            [ toTop location, toBottom location, toLeft location ]

        RightT ->
            [ toTop location, toRight location, toBottom location ]

        TopLeftConnector ->
            [ toTop location, toLeft location ]

        TopBottomConnector ->
            [ toTop location, toBottom location ]

        TopRightConnector ->
            [ toTop location, toRight location ]

        TopT ->
            [ toTop location, toRight location, toLeft location ]


toTop : Location -> ( Location, Direction )
toTop location =
    ( Location (location.row - 1) location.column, Bottom )


toRight : Location -> ( Location, Direction )
toRight location =
    ( Location location.row (location.column + 1), Left )


toBottom : Location -> ( Location, Direction )
toBottom location =
    ( Location (location.row + 1) location.column, Top )


toLeft : Location -> ( Location, Direction )
toLeft location =
    ( Location location.row (location.column - 1), Right )


rotateRightPipe : ( Pipe, List Option ) -> ( Pipe, List Option )
rotateRightPipe ( pipe, options ) =
    case pipe of
        Opening openingDirection openingType ->
            case openingDirection of
                Top ->
                    ( Opening Right openingType, options )

                Right ->
                    ( Opening Bottom openingType, options )

                Bottom ->
                    ( Opening Left openingType, options )

                Left ->
                    ( Opening Top openingType, options )

        Connector4 ->
            ( Connector4, options )

        BottomLeftConnector ->
            ( TopLeftConnector, options )

        BottomRightConnector ->
            ( BottomLeftConnector, options )

        BottomT ->
            ( LeftT, options )

        LeftRightConnector ->
            ( TopBottomConnector, options )

        LeftT ->
            ( TopT, options )

        RightT ->
            ( BottomT, options )

        TopLeftConnector ->
            ( TopRightConnector, options )

        TopBottomConnector ->
            ( LeftRightConnector, options )

        TopRightConnector ->
            ( BottomRightConnector, options )

        TopT ->
            ( RightT, options )


view : Model -> Html Msg
view model =
    div []
        [ viewBoard model
        , viewControls
        ]


viewControls : Html Msg
viewControls =
    div [ classes [ mt3 ] ]
        [ button [ classes validateButtonClasses, onClick Attempt ] [ text "Open water" ]
        ]


viewBoard : Model -> Html Msg
viewBoard model =
    let
        columnsStyle =
            Matrix.width model.gameBoard
                |> (\count -> String.repeat count "1fr ")
                |> String.trimRight
    in
    Matrix.indexedMap (\column row value -> ( Location row column, value )) model.gameBoard
        |> Matrix.toArray
        |> Array.toList
        |> List.map viewPipe
        |> div [ classes [ "water-puzzle" ], style "grid-template-columns" columnsStyle ]


toPolygon : String -> Svg Msg
toPolygon pointsString =
    polygon [ points pointsString, fill "gray", stroke "black", strokeWidth "5" ] []


viewPipe : ( Location, ( Pipe, List Option ) ) -> Html Msg
viewPipe ( location, ( pipe, options ) ) =
    let
        svgElements =
            case pipe of
                Opening direction openingType ->
                    viewOpening direction openingType

                Connector4 ->
                    "100,-5 100,100 -5,100 -5,200, 100,200, 100,305 200,305 200,200 305,200 305,100 200,100 200,-5"
                        |> toPolygon
                        |> List.singleton

                BottomLeftConnector ->
                    "-5,100 200,100 200,305 100,305 100,200 -5,200"
                        |> toPolygon
                        |> List.singleton

                BottomRightConnector ->
                    "305,100 100,100 100,305 200,305 200,200 305,200"
                        |> toPolygon
                        |> List.singleton

                BottomT ->
                    "-5,100 305,100 305,200 200,200 200,305 100,305 100,200 -5,200"
                        |> toPolygon
                        |> List.singleton

                LeftRightConnector ->
                    "-5,100 305,100 305,200 -5,200"
                        |> toPolygon
                        |> List.singleton

                LeftT ->
                    "200,-5 200,305 100,305 100,200 -5,200 -5,100 100,100 100,-5"
                        |> toPolygon
                        |> List.singleton

                RightT ->
                    "100,-5 100,305 200,305 200,200 305,200 305,100 200,100 200,-5"
                        |> toPolygon
                        |> List.singleton

                TopLeftConnector ->
                    "200,-5 200,200 -5,200 -5,100 100,100 100,-5"
                        |> toPolygon
                        |> List.singleton

                TopBottomConnector ->
                    "100,-5 100,305 200,305 200,-5"
                        |> toPolygon
                        |> List.singleton

                TopRightConnector ->
                    "100,-5 100,200 305,200 305,100 200,100 200,-5"
                        |> toPolygon
                        |> List.singleton

                TopT ->
                    "-5,200 305,200 305,100 200,100 200,-5 100,-5 100,100 -5,100"
                        |> toPolygon
                        |> List.singleton

        onClickAttributes =
            if List.member NotRotatable options then
                [ class "pipe-tile" ]

            else
                [ onClick (Rotate location), class ("pipe-tile " ++ pointer) ]
    in
    [ viewBox "0 0 300 300", strokeWidth "3" ]
        |> (\attributes -> svg attributes svgElements)
        |> List.singleton
        |> div onClickAttributes


viewOpening : Direction -> OpeningType -> List (Svg Msg)
viewOpening direction openingType =
    let
        polygonPoints =
            case direction of
                Top ->
                    "200,-5 200,130 220,130 220,150 80,150 80,130 100,130 100,-5"

                Right ->
                    "305,200 170,200 170,220 150,220 150,80 170,80 170,100 305,100"

                Bottom ->
                    "100,305 100,170 80,170 80,150 220,150 220,170 200,170 200,305"

                Left ->
                    "-5,100 130,100 130,80 150,80 150,220 130,220 130,200 -5,200"
    in
    viewArrow openingType direction
        |> List.append (toPolygon polygonPoints |> List.singleton)


viewArrow : OpeningType -> Direction -> List (Svg Msg)
viewArrow openingType openingDirection =
    case ( openingType, openingDirection ) of
        ( Entrance, Top ) ->
            [ line [ x1 "150", y1 "180", x2 "150", y2 "270", stroke "blue", strokeWidth "10px" ] []
            , line [ x1 "152.5", y1 "170", x2 "120", y2 "210", stroke "blue", strokeWidth "10px" ] []
            , line [ x1 "147.5", y1 "170", x2 "180", y2 "210", stroke "blue", strokeWidth "10px" ] []
            ]

        ( Exit, Top ) ->
            [ line [ x1 "150", y1 "270", x2 "150", y2 "180", stroke "blue", strokeWidth "10px" ] []
            , line [ x1 "152.5", y1 "280", x2 "120", y2 "250", stroke "blue", strokeWidth "10px" ] []
            , line [ x1 "147.5", y1 "280", x2 "180", y2 "250", stroke "blue", strokeWidth "10px" ] []
            ]

        ( Entrance, Bottom ) ->
            [ line [ x1 "150", y1 "120", x2 "150", y2 "30", stroke "blue", strokeWidth "10px" ] []
            , line [ x1 "152.5", y1 "130", x2 "120", y2 "90", stroke "blue", strokeWidth "10px" ] []
            , line [ x1 "147.5", y1 "130", x2 "180", y2 "90", stroke "blue", strokeWidth "10px" ] []
            ]

        ( Exit, Bottom ) ->
            [ line [ x1 "150", y1 "30", x2 "150", y2 "120", stroke "blue", strokeWidth "10px" ] []
            , line [ x1 "152.5", y1 "20", x2 "120", y2 "50", stroke "blue", strokeWidth "10px" ] []
            , line [ x1 "147.5", y1 "20", x2 "180", y2 "50", stroke "blue", strokeWidth "10px" ] []
            ]

        ( Entrance, Left ) ->
            [ line [ x1 "180", y1 "150", x2 "270", y2 "150", stroke "blue", strokeWidth "10px" ] []
            , line [ x1 "170", y1 "152.5", x2 "210", y2 "120", stroke "blue", strokeWidth "10px" ] []
            , line [ x1 "170", y1 "147.5", x2 "210", y2 "180", stroke "blue", strokeWidth "10px" ] []
            ]

        ( Exit, Left ) ->
            [ line [ x1 "270", y1 "150", x2 "180", y2 "150", stroke "blue", strokeWidth "10px" ] []
            , line [ x1 "280", y1 "152.5", x2 "250", y2 "120", stroke "blue", strokeWidth "10px" ] []
            , line [ x1 "280", y1 "147.5", x2 "250", y2 "180", stroke "blue", strokeWidth "10px" ] []
            ]

        ( Entrance, Right ) ->
            [ line [ x1 "120", y1 "150", x2 "30", y2 "150", stroke "blue", strokeWidth "10px" ] []
            , line [ x1 "130", y1 "152.5", x2 "90", y2 "120", stroke "blue", strokeWidth "10px" ] []
            , line [ x1 "130", y1 "147.5", x2 "90", y2 "180", stroke "blue", strokeWidth "10px" ] []
            ]

        ( Exit, Right ) ->
            [ line [ x1 "20", y1 "150", x2 "120", y2 "150", stroke "blue", strokeWidth "10px" ] []
            , line [ x1 "20", y1 "152.5", x2 "50", y2 "120", stroke "blue", strokeWidth "10px" ] []
            , line [ x1 "20", y1 "147.5", x2 "50", y2 "180", stroke "blue", strokeWidth "10px" ] []
            ]
