module Attempt exposing (AttemptPenalty(..), AttemptStatus(..))

import Timer.Timer exposing (Milliseconds)


type AttemptStatus
    = Correct
    | Incorrect


type AttemptPenalty
    = RemoveTime Milliseconds
