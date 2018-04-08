port module Main exposing (..)

import Html exposing (Html, text, div, h1, h2, h3, img, input, button)
import Set as S
import List as L
import Maybe exposing (map)
import WebSocket as WS
import Model exposing (..)
import Time exposing (..)
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Row as Row
import Bootstrap.Grid.Col as Col


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( { error = Nothing
      , users = S.empty -- TODO List
      , time = 0
      }
    , wsMessageOut (joining "NEW")
    )


port windowFocus : (String -> msg) -> Sub msg


wsURL : String
wsURL =
    "ws://localhost:9160/"


wsMessageOut : String -> Cmd msg
wsMessageOut msg =
    WS.send wsURL msg



---- UPDATE ----


joining : String -> String
joining name =
    "JOIN " ++ name


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick newtime ->
            ( { model | time = newtime }, Cmd.none )

        NewMessage name ->
            ( { model | users = S.insert name model.users }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ WS.listen wsURL NewMessage
        , Time.every second Tick
        ]



---- VIEW ----


view : Model -> Html Msg
view model =
    Grid.container []
        [ Grid.row [ Row.centerXs ]
            [ Grid.col [ Col.xs12 ]
                [ h3 [] [ text "HQ" ]
                ]
            ]
        , h2 [] [ text "Benutzer" ]
        , Html.div [] (List.map (\name -> Html.div [] [ Html.text name ]) (S.toList model.users))
        , Html.div []
            [ case model.error of
                Just err ->
                    h3 [] [ text err ]

                Nothing ->
                    text ""
            ]
        ]



---- PROGRAM ----


main : Program Flags Model Msg
main =
    Html.programWithFlags
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
