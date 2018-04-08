module Model exposing (..)

import Set as S
import Time exposing (..)


type alias Flags =
    {}


type alias Model =
    { error : Maybe String
    , users : S.Set Name
    , time : Time
    }


type alias Name =
    String


type Msg
    = NewMessage String
    | Tick Time



-------------------------------------------------------------------------


type alias WsMsg =
    String
