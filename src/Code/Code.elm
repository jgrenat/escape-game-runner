module Code.Code exposing (Model, Msg, init, update, askCodeView)

import Char exposing (isDigit)
import Html exposing (Html, button, div, fieldset, form, input, label, legend, p, text)
import Html.Attributes exposing (autocomplete, autofocus, for, id, name, type_, value)
import Html.Events exposing (onInput, onSubmit)
import String exposing (toInt)
import Tachyons exposing (classes)
import Tachyons.Classes exposing (b__black_10, ba, bg_animate, bg_black_70, bg_green, bg_white, black_80, bn, border_box, br2_ns, br__left_ns, br__right_ns, button_reset, center, cf, clip, f4_ns, f5, f5_l, f6, fl, hover_bg_black, input_reset, lh_solid, ma0, mb3, mw7, pa0, pa3, pa4, pointer, pv3, tc, tl, w_100, w_20_l, w_25_m, w_75_m, w_80_l, white)


type Msg
    = CodeAttempt
    | OnCodeChanged String


init : Model
init =
    Model ""


update : Msg -> Model -> ( Model, Maybe String )
update msg model =
    case msg of
        OnCodeChanged newCode ->
            let
                onlyDigits =
                    String.filter isDigit newCode
            in
                ( { model | codeInput = onlyDigits }, Nothing )

        CodeAttempt ->
            ( model, Just model.codeInput )


type alias Model =
    { codeInput : String }


askCodeView : Model -> Html Msg
askCodeView model =
    form [ onSubmit CodeAttempt, classes [ mw7, center, pa4, ba, br2_ns, b__black_10, bg_green, mb3 ] ]
        [ fieldset [ classes [ cf, bn, ma0, pa0 ] ]
            [ legend [ classes [ pa0, f5, f4_ns, mb3, white, tl ] ] [ text "Enter a code" ]
            , div
                [ classes [ cf ] ]
                [ label [ classes [ clip ], for "machineId" ] [ text "Code" ]
                , input [ autocomplete False, autofocus True, classes [ border_box, f6, bn, f5_l, input_reset, fl, black_80, bg_white, pa3, lh_solid, w_100, w_75_m, w_80_l, br2_ns, br__left_ns ], id "machineId", value model.codeInput, onInput OnCodeChanged ] []
                , input [ type_ "submit", classes [ f6, f5_l, button_reset, fl, pv3, tc, bn, bg_animate, bg_black_70, hover_bg_black, white, pointer, w_100, w_25_m, w_20_l, br2_ns, br__right_ns ] ] [ text "Try" ]
                ]
            ]
        ]
