module Main exposing (..)

-- File upload with the <input type="file"> node.
--
-- Dependencies:
--   elm install elm/file
--   elm install elm/json
--

import Browser
import Decoder exposing (..)
import File exposing (File)
import File.Select exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode exposing (..)
import Task



-- MAIN


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    ( String, Decoder.Model )


init : () -> ( Model, Cmd Msg )
init _ =
    ( ( "rien", Decoder.empty_model ), Cmd.none )



-- UPDATE


type Msg
    = GotFile File
    | ParsedFile String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ( _, model ) =
    case msg of
        GotFile file ->
            ( ( "gotfile", model ), Task.perform ParsedFile (File.toString file) )

        ParsedFile content ->
            ( ( "parsed", Decoder.parse_content content ), Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ input
            [ type_ "file"
            , multiple False
            , on "change" (Json.Decode.map GotFile File.decoder)
            ]
            []
        , div [] [ text (Debug.toString model) ]
        ]
