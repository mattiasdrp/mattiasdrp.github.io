module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Events exposing (onClick)
import Pythagoras_tree exposing (view_pythagoras_tree)
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
    , gamma : SingleSlider.SingleSlider Msg
    }


init : Model
init =
    { depth = 2
    , alpha = SingleSlider.init { min = 0, max = 360, value = 60, step = 1, onChange = AlphaChange }
    , gamma = SingleSlider.init { min = 0, max = 360, value = 60, step = 1, onChange = GammaChange }
    }



-- UPDATE


type Msg
    = IncrementDepth
    | DecrementDepth
    | AlphaChange Float
    | GammaChange Float


update : Msg -> Model -> Model
update msg model =
    case msg of
        IncrementDepth ->
            if model.depth == 12 then
                model

            else
                { model | depth = model.depth + 1 }

        DecrementDepth ->
            if model.depth == 1 then
                model

            else
                { model | depth = model.depth - 1 }

        AlphaChange angle ->
            { model | alpha = SingleSlider.update angle model.alpha }

        GammaChange angle ->
            { model | gamma = SingleSlider.update angle model.gamma }



-- UTILS


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick DecrementDepth ] [ text "-" ]
        , div [] [ text (String.fromFloat model.depth) ]
        , button [ onClick IncrementDepth ] [ text "+" ]
        , div [] [ SingleSlider.view model.alpha, SingleSlider.view model.gamma ]
        , div [] [ view_pythagoras_tree (SingleSlider.fetchValue model.alpha) (SingleSlider.fetchValue model.gamma) model.depth ]
        ]
