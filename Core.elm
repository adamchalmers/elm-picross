module Core exposing (..)
import Grid exposing (Grid)

type alias Model =
    { puzzle : Grid Bool
    , progress : Grid Bool
    , console : String
    }

type Msg = Noop | Click Int Int

