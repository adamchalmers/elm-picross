module Core exposing (..)
import Grid exposing (Grid)

type alias Model =
    { puzzle : Grid Bool
    , progress : Grid Mark
    , console : String
    }

type Msg = Noop | Click Int Int
type Mark = White | Black | Dot
type HintProgress = Todo | Done | Mistake

type alias Hint = (Int, HintProgress)