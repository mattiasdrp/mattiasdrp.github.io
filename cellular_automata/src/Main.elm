module Main exposing (..)

-- import Browser.Navigation

import Array exposing (Array)
import Binary exposing (..)
import Browser
import Browser.Dom
import Browser.Events
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Maybe exposing (withDefault)
import Random exposing (Seed)
import Task exposing (..)
import Time exposing (..)


type alias Model =
    { array : Array Int
    , random : Bool
    , size : Int
    , square_size : Int
    , height : Int
    , seed : Random.Seed
    , rule : Array Int
    , temp_rule : String
    , prev_arrays : List (Array Int)
    , start : Bool
    , temp_alive_color : String
    , alive_color : String
    , temp_dead_color : String
    , dead_color : String
    }


main : Program Int Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init_array : Int -> Array Int
init_array size =
    Array.repeat size 0
        |> Array.set (size // 2) 1


random_array : Seed -> Int -> ( Array Int, Seed )
random_array seed size =
    let
        gen =
            Random.list size <| Random.int 0 1

        ( list, sseed ) =
            Random.step gen seed
    in
    ( Array.fromList list, sseed )


gen_array : Model -> Model
gen_array model =
    let
        ( array, sseed ) =
            if model.random then
                random_array model.seed model.size

            else
                ( init_array model.size, model.seed )
    in
    { model | array = array, seed = sseed, prev_arrays = [] }


init : Int -> ( Model, Cmd Msg )
init iseed =
    let
        seed =
            Random.initialSeed iseed

        size =
            201

        model =
            { array = Array.empty
            , random = False
            , size = size
            , square_size = 0
            , height = 0
            , seed = seed
            , rule = Array.fromList [ 0, 1, 1, 0, 1, 1, 1, 0 ]
            , temp_rule = ""
            , prev_arrays = []
            , start = False
            , temp_alive_color = ""
            , alive_color = "black"
            , temp_dead_color = ""
            , dead_color = "white"
            }

        mmodel =
            gen_array model
    in
    ( mmodel
    , Task.perform Sizes Browser.Dom.getViewport
    )


type Msg
    = ChangedRule
    | RuleInputChanged String
    | Tick
    | Toggle
    | Reset
    | Randomize
    | Deterministic
    | Sizes Browser.Dom.Viewport
    | Width Int
    | AliveColorInputChanged String
    | AliveColor
    | DeadColorInputChanged String
    | DeadColor


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RuleInputChanged s ->
            ( { model | temp_rule = s }, Cmd.none )

        ChangedRule ->
            let
                mmodel =
                    temp_rule_to_model model
            in
            ( mmodel, Cmd.none )

        Tick ->
            if model.start then
                let
                    mmodel =
                        { model
                            | height = model.height + 1
                            , array = update_array model
                        }
                in
                ( { mmodel | prev_arrays = model.array :: model.prev_arrays }
                , Cmd.none
                )

            else
                ( model, Cmd.none )

        Toggle ->
            ( if model.start then
                { model
                    | start = False
                }

              else
                { model | start = True }
            , Cmd.none
            )

        Randomize ->
            let
                mmodel =
                    { model | random = True }
            in
            ( gen_array mmodel, Cmd.none )

        Deterministic ->
            let
                mmodel =
                    { model | random = False }
            in
            ( gen_array mmodel, Cmd.none )

        Reset ->
            ( reset_model model
            , Cmd.none
            )

        Sizes vp ->
            let
                width =
                    round vp.viewport.width
            in
            ( update_square_size model width, Cmd.none )

        AliveColorInputChanged s ->
            ( { model | temp_alive_color = s }, Cmd.none )

        AliveColor ->
            ( { model | alive_color = model.temp_alive_color }, Cmd.none )

        DeadColorInputChanged s ->
            ( { model | temp_dead_color = s }, Cmd.none )

        DeadColor ->
            ( { model | dead_color = model.temp_dead_color }, Cmd.none )

        Width width ->
            ( update_square_size model width, Cmd.none )


reset_model : Model -> Model
reset_model model =
    gen_array model


update_square_size : Model -> Int -> Model
update_square_size model width =
    let
        square_size =
            width // model.size
    in
    { model | square_size = square_size }


aget : Int -> Array number -> Int -> number
aget index array size =
    Array.get (modBy size index) array
        |> withDefault -1


update_array : Model -> Array Int
update_array model =
    let
        array =
            Array.indexedMap
                (\index c ->
                    let
                        lookat =
                            7
                                - (Binary.fromIntegers
                                    [ aget (index - 1) model.array model.size
                                    , c
                                    , aget (index + 1) model.array model.size
                                    ]
                                    |> Binary.toDecimal
                                  )
                    in
                    Array.get lookat model.rule |> withDefault -1
                )
                model.array
    in
    array


temp_rule_to_model : Model -> Model
temp_rule_to_model model =
    let
        rule =
            Maybe.withDefault 110 (String.toInt model.temp_rule)
                |> Binary.fromDecimal
                -- [1, 0, 1, 0, 1, 0]
                |> Binary.toIntegers
                -- [1, 0, 1, 0, 1, 0]
                |> List.map String.fromInt
                -- ["1", "0", "1", "0", "1", "0"]
                |> String.join ""
                -- "101010"
                |> String.padLeft 8 '0'
                -- "00101010"
                |> String.toList
                -- ["0", "0", "1", "0", "1", "0", "1", "0"]
                |> List.map
                    (\c ->
                        if c == '0' then
                            0

                        else
                            1
                    )
                -- [0, 0, 1, 0, 1, 0, 1, 0]
                |> Array.fromList
    in
    { model | rule = rule }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Time.every 100 (\_ -> Tick)
        , Browser.Events.onResize (\w _ -> Width w)
        ]


view : Model -> Html Msg
view model =
    div []
        [ input
            [ placeholder "Rule (0-255)"
            , Html.Attributes.value model.temp_rule
            , onInput RuleInputChanged
            ]
            []
        , Html.text " "
        , button [ onClick ChangedRule ] [ Html.text "OK" ]
        , Html.text " "
        , button [ onClick Toggle ]
            [ text
                (if model.start then
                    "Stop"

                 else
                    "Start"
                )
            ]
        , Html.text " "
        , button [ onClick Reset ] [ text "Reset" ]
        , Html.text " "
        , button [ onClick Randomize ] [ text "Random" ]
        , Html.text " "
        , button [ onClick Deterministic ]
            [ text "Deterministic" ]
        , Html.text " "
        , input
            [ placeholder "Alive Colour"
            , Html.Attributes.value model.temp_alive_color
            , onInput AliveColorInputChanged
            ]
            []
        , Html.text " "
        , button [ onClick AliveColor ] [ Html.text "OK" ]
        , Html.text " "
        , input
            [ placeholder "Dead Colour"
            , Html.Attributes.value model.temp_dead_color
            , onInput DeadColorInputChanged
            ]
            []
        , Html.text " "
        , button [ onClick DeadColor ] [ Html.text "OK" ]
        , div [] (List.map (render_array model) (List.reverse (model.array :: model.prev_arrays)))
        ]


render_array : Model -> Array Int -> Html Msg
render_array model array =
    let
        res =
            Array.foldl
                (\value list ->
                    square value model :: list
                )
                []
                array
    in
    div
        [ style "display" "grid"
        , style "gap" "0"
        , style "grid-template-columns" ("repeat(" ++ String.fromInt model.size ++ "," ++ String.fromInt model.square_size ++ "px)")
        ]
        res


square : Int -> Model -> Html Msg
square value model =
    div
        [ style "width" (String.fromInt model.square_size ++ "px")
        , style "height" (String.fromInt model.square_size ++ "px")
        , style "border" "none"
        , style "background-color"
            (if value == 1 then
                model.alive_color

             else
                model.dead_color
            )
        ]
        [ text "" ]
