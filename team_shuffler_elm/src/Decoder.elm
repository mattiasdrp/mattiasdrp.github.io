module Decoder exposing (Model, empty_model,parse_content)

import Csv.Decode exposing (..)
import Dict exposing (Dict, empty)


lineDecoder : Decoder ( String, String )
lineDecoder =
    into (\info nom -> ( info, nom ))
        |> pipeline (column 0 string)
        |> pipeline (column 2 string)


decode : String -> Result Error (List ( String, String ))
decode lines =
    decodeCustom { fieldSeparator = ';' }
        NoFieldNames
        lineDecoder
        lines


type alias Joueuse =
    { position : String
    , groupe : String
    , niveau : String
    }


type alias State =
    { niveau : Maybe String
    , groupe : Maybe String
    , position : Maybe String
    , joueuses : Dict String Joueuse
    }

type alias Model = Dict String Joueuse

empty_model:Model
empty_model = Dict.empty

empty_state : State
empty_state =
    { niveau = Nothing, groupe = Nothing, position = Nothing, joueuses = Dict.empty }


parse_line : State -> ( String, String ) -> State
parse_line state ( info, nom ) =
    if state.niveau == Nothing then
        { state | niveau = Just (String.trim info) }
    else if state.groupe == Nothing then
        { state | groupe = Just (String.trim info) }
    else
        case ( String.trim info, String.trim nom ) of
            ( "", "" ) ->
                { state | niveau = Nothing, groupe = Nothing, position = Nothing }
            ( ngroupe, "" ) ->
                { state | groupe = Just ngroupe }
            ( "", nnom ) ->
                let
                    joueuse =
                        { position = Maybe.withDefault "NO POS" state.position, groupe = Maybe.withDefault "" state.groupe, niveau = Maybe.withDefault "" state.niveau }
                in
                { state | joueuses = Dict.insert nnom joueuse state.joueuses }
            ( nposition, nnom ) ->
                let
                    joueuse =
                        { position = nposition, groupe = Maybe.withDefault "" state.groupe, niveau = Maybe.withDefault "" state.niveau }
                in
                { state | position = Just nposition, joueuses = Dict.insert nnom joueuse state.joueuses }


parse_list : List ( String, String ) -> State -> Model
parse_list list state =
    case list of
        [] ->
            state.joueuses
        hd :: tl ->
            parse_list tl (parse_line state hd)


parse_content : String -> Model
parse_content lines =
    case decode lines of
        Ok list ->
            parse_list list empty_state
        Err _ ->
            parse_list [] empty_state
