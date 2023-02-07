module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Events exposing (onClick)
import SingleSlider exposing (..)
import TypedSvg exposing (..)
import TypedSvg.Attributes exposing (..)
import TypedSvg.Types exposing (..)



-- MAIN


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
    { depth : Float
    , alpha : SingleSlider.SingleSlider Msg
    }


init : Model
init =
    { depth = 2, alpha = SingleSlider.init { min = 10, max = 80, value = 45, step = 1, onChange = AlphaChange } }



-- UPDATE


type Msg
    = IncrementDepth
    | DecrementDepth
    | AlphaChange Float


update : Msg -> Model -> Model
update msg model =
    case msg of
        IncrementDepth ->
            { model | depth = model.depth + 1 }

        DecrementDepth ->
            if model.depth == 1 then
                model

            else
                { model | depth = model.depth - 1 }

        AlphaChange angle ->
            { model | alpha = SingleSlider.update angle model.alpha }



-- UTILS


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick DecrementDepth ] [ text "-" ]
        , div [] [ text (String.fromFloat model.depth) ]
        , button [ onClick IncrementDepth ] [ text "+" ]
        , div [] [ SingleSlider.view model.alpha ]
        , div [] [ view_pythagoras_tree model ]
        ]
