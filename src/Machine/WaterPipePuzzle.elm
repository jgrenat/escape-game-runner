module Machine.WaterPipePuzzle exposing (Pipe(..), Model, Msg, init, update, view, PipeWithOptions, GameBoard, Option(..), OpeningType(..), Direction(..))

import Html exposing (Html, beginnerProgram, button, div, i, img, text)
import Html.Events exposing (onClick)
import Matrix exposing (Location, Matrix, col, loc, row)
import Styles.Styles exposing (defaultButtonClasses, validateButtonClasses)
import Svg exposing (Svg, line, polygon, svg)
import Svg.Attributes exposing (class, fill, height, points, stroke, strokeLinejoin, strokeWidth, viewBox, width, x1, x2, y1, y2)
import Tachyons exposing (classes)
import Tachyons.Classes exposing (mt3, pointer)
import Attempt exposing (AttemptStatus(..))


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


type Msg
    = Rotate Location
    | Attempt


init : List (List ( Pipe, List Option )) -> Model
init pipesWithOptions =
    Matrix.fromList pipesWithOptions |> Model


update : Msg -> Model -> ( Model, Maybe AttemptStatus )
update msg model =
    case msg of
        Rotate location ->
            let
                updatedBoard =
                    Matrix.get location model.gameBoard
                        |> Maybe.map rotateRightPipe
                        |> Maybe.map (\rotatedPipe -> Matrix.set location rotatedPipe model.gameBoard)
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
                |> List.foldl propagateWaterIntoPipe wateredGameBoard
                |> Matrix.toList
                |> List.concat
                |> List.map Tuple.second
    in
        (not <| List.member Flooded resultingPipesStates)
            && List.member Out resultingPipesStates


getEntrances : GameBoard -> List ( Location, Direction )
getEntrances gameBoard =
    Matrix.mapWithLocation (\location ( pipe, _ ) -> ( location, pipe )) gameBoard
        |> Matrix.toList
        |> List.concat
        |> List.filterMap
            (\( location, pipe ) ->
                case pipe of
                    Opening openingDirection Entrance ->
                        Just ( location, openingDirection )

                    _ ->
                        Nothing
            )


markPipeAsVisited : WateredGameBoard -> Location -> WateredGameBoard
markPipeAsVisited wateredGameBoard location =
    wateredGameBoard
        |> Matrix.update location
            (\( pipe, waterState ) ->
                case waterState of
                    NotVisited ->
                        ( pipe, NothingSpecial )

                    status ->
                        ( pipe, status )
            )


propagateWaterIntoPipe : ( Location, Direction ) -> WateredGameBoard -> WateredGameBoard
propagateWaterIntoPipe ( location, direction ) wateredBoard =
    case Matrix.get location wateredBoard of
        Just ( pipe, state ) ->
            if not <| (isAccessibleBy direction pipe) then
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
                    Matrix.update previousLocation (\( pipe, _ ) -> ( pipe, Flooded )) wateredBoard
            else
                case ( pipe, state ) of
                    ( Opening _ Exit, NotVisited ) ->
                        Matrix.update location (\( pipe, _ ) -> ( pipe, Out )) wateredBoard

                    ( pipe, NotVisited ) ->
                        let
                            newWateredBoard =
                                Matrix.update location (\_ -> ( pipe, NothingSpecial )) wateredBoard

                            exits : List ( Location, Direction )
                            exits =
                                getPipeExits location pipe
                        in
                            List.foldl propagateWaterIntoPipe newWateredBoard exits

                    ( pipe, _ ) ->
                        wateredBoard

        Nothing ->
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
                Matrix.update previousLocation (\( pipe, _ ) -> ( pipe, Flooded )) wateredBoard


isAccessibleBy : Direction -> Pipe -> Bool
isAccessibleBy direction pipe =
    let
        exits =
            getPipeExits (loc 0 0) pipe |> List.map Tuple.second

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
    ( loc (row location - 1) (col location), Bottom )


toRight : Location -> ( Location, Direction )
toRight location =
    ( loc (row location) (col location + 1), Left )


toBottom : Location -> ( Location, Direction )
toBottom location =
    ( loc (row location + 1) (col location), Top )


toLeft : Location -> ( Location, Direction )
toLeft location =
    ( loc (row location) (col location - 1), Right )


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
    Matrix.mapWithLocation (,) model.gameBoard
        |> Matrix.toList
        |> List.concatMap (List.map viewPipe)
        |> div [ classes [ "water-puzzle" ] ]


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
            if (List.member NotRotatable options) then
                [ class "pipe-tile" ]
            else
                [ onClick (Rotate location), class ("pipe-tile " ++ pointer) ]
    in
        [ viewBox "0 0 300 300", strokeWidth "3" ]
            |> List.append onClickAttributes
            |> (\attributes -> svg attributes svgElements)


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
