module Main exposing (..)

-- import Debug exposing (toString)

import Array exposing (Array)
import Binary exposing (..)
import Browser
import Browser.Navigation exposing (back)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Maybe exposing (withDefault)
import Random exposing (Seed)
import Time exposing (..)


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


square_size =
    20


init_size =
    101


init_array size =
    Array.repeat size 0
        |> Array.set (size // 2) 1


type alias Model =
    { base_array : Array Int
    , array : Array Int
    , size : Int
    , height : Int
    , seed : Random.Seed
    , rule : Array Int
    , temp_rule : String
    , rendered_arrays : List (Html Msg)
    , start : Bool
    }


init : Int -> ( Model, Cmd Msg )
init iseed =
    let
        seed =
            Random.initialSeed iseed

        gen =
            Random.list init_size <| Random.int 0 1

        ( list, sseed ) =
            Random.step gen seed

        base_array =
            init_array init_size

        -- Array.fromList list
    in
    ( { base_array = base_array
      , array = base_array

      -- array = init_array init_size
      , size = init_size
      , height = 0
      , seed = seed
      , rule = Array.fromList [ 0, 0, 0, 0, 0, 0, 0, 0 ]
      , temp_rule = ""
      , rendered_arrays = []
      , start = False
      }
    , Cmd.none
    )


type Msg
    = ChangedRule
    | RuleInputChanged String
    | Tick
    | Toggle


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
                ( { model
                    | height = model.height + 1
                    , array = update_array model
                    , rendered_arrays = rendered_array model :: model.rendered_arrays
                  }
                , Cmd.none
                )

            else
                ( model, Cmd.none )

        Toggle ->
            ( if model.start then
                { model
                    | rendered_arrays = []
                    , array = model.base_array
                    , start = False
                }

              else
                { model | start = True }
            , Cmd.none
            )


aget index array size =
    Array.get (modBy size index) array
        |> withDefault -1


update_array model =
    let
        array =
            Array.indexedMap
                (\index c ->
                    let
                        -- _ =
                        --     Debug.log "prev: " (aget (index - 1) model.array model.size)
                        -- _ =
                        --     Debug.log "curr: " c
                        -- _ =
                        --     Debug.log "next: " (aget (index + 1) model.array model.size)
                        lookat =
                            7
                                - (Binary.fromIntegers
                                    [ aget (index - 1) model.array model.size
                                    , c
                                    , aget (index + 1) model.array model.size
                                    ]
                                    |> Binary.toDecimal
                                  )

                        -- _ =
                        --     Debug.log "index: " index
                        -- _ =
                        --     Debug.log "bits: "
                        --         (Binary.fromIntegers
                        --             [ aget (index - 1) model.array model.size
                        --             , c
                        --             , aget (index + 1) model.array model.size
                        --             ]
                        --         )
                        -- _ =
                        --     Debug.log "lookat: " lookat
                    in
                    Array.get lookat model.rule |> withDefault -1
                )
                model.array

        -- _ =
        --     Debug.log "oarray: " model.array
        -- _ =
        --     Debug.log "narray: " array
    in
    array


temp_rule_to_model : Model -> Model
temp_rule_to_model model =
    let
        rule =
            Maybe.withDefault 0 (String.toInt model.temp_rule)
                |> Binary.fromDecimal
                -- [1, 0, 1, 0, 1, 0]
                |> Binary.toIntegers
                -- [1, 0, 1, 0, 1, 0]
                |> List.map String.fromInt
                -- ["1", "0", "1", "0", "1", "0"]
                |> String.join ""
                -- "101010"
                |> String.padLeft 8 '0'
                |> String.toList
                |> List.map
                    (\c ->
                        if c == '0' then
                            0

                        else
                            1
                    )
                |> Array.fromList

        -- _ =
        --     Debug.log "rule: " rule
    in
    { model | rule = rule }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every 100 (\_ -> Tick)


view : Model -> Html Msg
view model =
    div []
        [ input
            [ placeholder "Rule"
            , Html.Attributes.value model.temp_rule
            , onInput RuleInputChanged
            ]
            []
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
        , div [] (List.reverse model.rendered_arrays)
        ]


rendered_array : Model -> Html Msg
rendered_array model =
    let
        ( _, res ) =
            Array.foldl (\value ( i, list ) -> ( i + 1, square ( value, model.height * square_size, i * square_size ) :: list )) ( 0, [] ) model.array
    in
    div
        [ style "display" "grid"
        , style "gap" "0"
        , style "grid-template-columns" ("repeat(" ++ String.fromInt model.size ++ "," ++ String.fromInt square_size ++ "px)")
        ]
        res


square : ( Int, Int, Int ) -> Html Msg
square ( value, col, row ) =
    div
        [ style "width" (String.fromInt square_size ++ "px")
        , style "height" (String.fromInt square_size ++ "px")
        , style "border" "none"
        , style "background-color"
            (if value == 1 then
                "black"

             else
                "white"
            )
        ]
        [ text "" ]
