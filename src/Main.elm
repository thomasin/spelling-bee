module Main exposing (..)

import Browser
import Html


--


type alias Model = {}


type Msg = NoOp


init : () -> ( Model, Cmd Msg )
init _ =
      ({}, Cmd.none)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            (model, Cmd.none)


view : Model -> Browser.Document Msg
view model =
    { title = "🌸"
    , body = [ Html.div [] [ Html.text "good morning" ] ]
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
