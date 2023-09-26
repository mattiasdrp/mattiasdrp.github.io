module Decoder exposing (Buckets, Groupes, empty_buckets, empty_groupes, generate_buckets, generate_groupes)

import Array exposing (Array)
import Csv.Decode exposing (..)
import Csv.Parser exposing (parse)
import Dict exposing (Dict)
import Random exposing (..)


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


type alias JDict =
    Dict String Joueuse


type alias GDict =
    Dict String (Dict String (List ( String, String )))


parse_list : List ( String, String ) -> State -> JDict
parse_list list state =
    case list of
        [] ->
            state.joueuses

        hd :: tl ->
            parse_list tl (parse_line state hd)


parse_content : String -> JDict
parse_content lines =
    case decode lines of
        Ok list ->
            parse_list list empty_state

        Err _ ->
            parse_list [] empty_state


jdict_to_gdict : JDict -> GDict
jdict_to_gdict jdict =
    Dict.foldl
        (\joueuse { position, groupe, niveau } groupes ->
            Dict.update groupe
                (\position_dict ->
                    case position_dict of
                        Nothing ->
                            Just (Dict.singleton position [ ( joueuse, niveau ) ])

                        Just pdict ->
                            Just
                                (Dict.update position
                                    (\joueuses ->
                                        case joueuses of
                                            Nothing ->
                                                Just [ ( joueuse, niveau ) ]

                                            Just l ->
                                                Just (( joueuse, niveau ) :: l)
                                    )
                                    pdict
                                )
                )
                groupes
        )
        Dict.empty
        jdict


type alias Groupes =
    Array ( String, Array ( String, Array ( String, String ) ) )


empty_groupes : Groupes
empty_groupes =
    Array.empty


gdict_to_array : GDict -> Groupes
gdict_to_array gdict =
    Dict.foldl
        (\groupe positions list ->
            let
                ppositions =
                    Dict.foldl
                        (\position joueuses acc ->
                            ( position, Array.fromList joueuses ) :: acc
                        )
                        []
                        positions
            in
            ( groupe, Array.fromList ppositions ) :: list
        )
        []
        gdict
        |> Array.fromList


type alias Buckets =
    Array (List ( ( String, String ), String, String ))


generate_buckets_aux : Array ( String, Array ( String, Array ( String, String ) ) ) -> Array (List ( ( String, String ), String, String )) -> Int -> Seed -> Int -> Array ( String, String ) -> Int -> String -> Int -> String -> Int -> ( Array (List ( ( String, String ), String, String )), Seed )
generate_buckets_aux groupes buckets nb_buckets seed curr_bucket joueuses j_size groupe id_groupes position id_position =
    if j_size == 0 then
        case Array.get id_groupes groupes of
            Just ( _, positions ) ->
                case Array.get (id_position + 1) positions of
                    Just ( pposition, jjoueuses ) ->
                        generate_buckets_aux groupes buckets nb_buckets seed curr_bucket jjoueuses (Array.length jjoueuses) groupe id_groupes pposition (id_position + 1)

                    Nothing ->
                        case Array.get (id_groupes + 1) groupes of
                            Just ( ggroupe, ppositions ) ->
                                case Array.get 0 ppositions of
                                    Just ( pposition, jjoueuses ) ->
                                        generate_buckets_aux groupes buckets nb_buckets seed curr_bucket jjoueuses (Array.length jjoueuses) ggroupe (id_groupes + 1) pposition 0

                                    Nothing ->
                                        ( buckets, seed )

                            Nothing ->
                                ( buckets, seed )

            Nothing ->
                ( buckets, seed )

    else
        let
            gen =
                Random.int 0 (j_size - 1)

            ( index_j, sseed ) =
                Random.step gen seed
        in
        case Array.get index_j joueuses of
            Just joueuse ->
                case Array.get (j_size - 1) joueuses of
                    Just njoueuse ->
                        let
                            jjoueuses =
                                Array.set index_j njoueuse joueuses

                            bbuckets =
                                case Array.get curr_bucket buckets of
                                    Just content ->
                                        Array.set curr_bucket (( joueuse, groupe, position ) :: content) buckets

                                    Nothing ->
                                        buckets
                        in
                        generate_buckets_aux groupes bbuckets nb_buckets sseed (modBy nb_buckets (curr_bucket + 1)) jjoueuses (j_size - 1) groupe id_groupes position id_position

                    Nothing ->
                        ( buckets, sseed )

            Nothing ->
                ( buckets, sseed )


empty_buckets : Buckets
empty_buckets =
    Array.empty


generate_groupes : String -> Groupes
generate_groupes content =
    parse_content content |> jdict_to_gdict |> gdict_to_array


generate_buckets : Int -> Groupes -> Seed -> ( Buckets, Seed )
generate_buckets nb_buckets groupes seed =
    let
        buckets =
            Array.initialize nb_buckets (always [])
    in
    case Array.get 0 groupes of
        Just ( groupe, positions ) ->
            case Array.get 0 positions of
                Just ( position, joueuses ) ->
                    generate_buckets_aux groupes buckets nb_buckets seed 0 joueuses (Array.length joueuses) groupe 0 position 0

                Nothing ->
                    ( buckets, seed )

        Nothing ->
            ( buckets, seed )
