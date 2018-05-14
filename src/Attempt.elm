module Attempt exposing (AttemptStatus(..), AttemptPenalty(..))

import Time exposing (Time)


type AttemptStatus
    = Correct
    | Incorrect


type AttemptPenalty
    = RemoveTime Time
