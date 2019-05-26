module Styles.Styles exposing (commonButtonClasses, defaultButtonClasses, inputClasses, labelClasses, validateButtonClasses)

import Tachyons.Classes exposing (b, b__black_20, ba, bg_black, bg_green, db, dib, dim, f6, input_reset, link, mb2, mh1, pa2, ph3, pointer, pv2, w_100, white)


commonButtonClasses : List String
commonButtonClasses =
    [ f6, link, dim, ph3, pv2, mb2, dib, white, bg_black, mh1, pointer ]


defaultButtonClasses : List String
defaultButtonClasses =
    white :: bg_black :: commonButtonClasses


validateButtonClasses : List String
validateButtonClasses =
    white :: bg_green :: commonButtonClasses


labelClasses : List String
labelClasses =
    [ f6, b, db, mb2 ]


inputClasses : List String
inputClasses =
    [ input_reset, ba, b__black_20, pa2, mb2, db, w_100 ]
