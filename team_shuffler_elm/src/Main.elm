module Main exposing (..)

-- File upload with the <input type="file"> node.
--
-- Dependencies:
--   elm install elm/file
--   elm install elm/json
--

import Array exposing (Array)
import Browser
import Decoder exposing (..)
import File exposing (File)
import File.Select exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode exposing (..)
import Random exposing (Seed)
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
    { buckets : Decoder.Buckets
    , joueuses : Decoder.Groupes
    , seed : Random.Seed
    , nb_buckets : Int
    , temp_buckets : String
    }


init : Int -> ( Model, Cmd Msg )
init seed =
    ( { buckets = Decoder.empty_buckets
      , joueuses = Decoder.empty_groupes
      , seed = Random.initialSeed seed
      , nb_buckets = 3
      , temp_buckets = ""
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = GotFiles (List File)
    | ParsedFile String
    | ChangeNbBuckets
    | InputChanged String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotFiles file ->
            case file of
                [ head ] ->
                    ( model, Task.perform ParsedFile (File.toString head) )

                _ ->
                    ( model, Cmd.none )

        ParsedFile content ->
            let
                joueuses =
                    Decoder.generate_groupes content

                mmodel =
                    temp_buckets_to_model model

                ( buckets, seed ) =
                    Decoder.generate_buckets mmodel.nb_buckets joueuses mmodel.seed
            in
            ( { mmodel
                | buckets = buckets
                , seed = seed
                , joueuses = joueuses
              }
            , Cmd.none
            )

        InputChanged s ->
            ( { model | temp_buckets = s }, Cmd.none )

        ChangeNbBuckets ->
            let
                mmodel =
                    temp_buckets_to_model model
            in
            if Array.isEmpty mmodel.joueuses then
                ( mmodel, Cmd.none )

            else
                let
                    ( buckets, seed ) =
                        Decoder.generate_buckets mmodel.nb_buckets mmodel.joueuses mmodel.seed
                in
                ( { mmodel
                    | buckets = buckets
                    , seed = seed
                  }
                , Cmd.none
                )


temp_buckets_to_model : Model -> Model
temp_buckets_to_model model =
    let
        nb_buckets =
            Maybe.withDefault 3 (String.toInt model.temp_buckets)
    in
    { model | nb_buckets = nb_buckets }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ --  text "Nombres d'équipes : "
          -- ,
          input
            [ placeholder "Nombre d'équipes"
            , Html.Attributes.value model.temp_buckets
            , onInput InputChanged
            ]
            []
        , text " "
        , button [ onClick ChangeNbBuckets ] [ text "Mélange" ]
        , text " "
        , input
            [ type_ "file"
            , multiple False
            , on "change" (Json.Decode.map GotFiles fileDecoder)
            ]
            []
        , if Array.isEmpty model.joueuses then
            div []
                [ h1 [] [ text "Fournir un fichier CSV de la forme suivante" ]
                , div []
                    (List.intersperse
                        (br [] [])
                        (List.map text (String.lines "BON NIVEAU;;;;;;;;;;;;;\nAVANTS;;;;;;;;;;;;;\nPILIERS;1;Maïna ANDRE\u{00A0}PINOTEAU;;;;;;;;;;;\n;2;Joséphine FARGEVIEILLE;;;;;;;;;;;\n;3;Zélia CAZE;;;;;;;;;;;\n;4;Sophie GREGOIRE;;;;;;;;;;;\n;5;Selma-May PASQUIER;;;;;;;;;;;\n;6;Evangeline RIVAL;;;;;;;;;;;\nTALONS;1;Manon BOURDIAL;;;;;;;;;;;\n;2;Ilayda KOCER;;;;;;;;;;;\n;3;Maud BABUT;;;;;;;;;;;\n2ème / 3ème Lignes;1;Flavie COLOMBET;;;;;;;;;;;\n;2;Maya GRODECOEUR;;;;;;;;;;;\n;3;Louise GUERIN;;;;;;;;;;;\n;4;Maéva LUI-HIN-TSAN;;;;;;;;;;;\n;5;Eva MAS;;;;;;;;;;;\n;6;Anne\u{00A0}Mary SOCK;;;;;;;;;;;\n;7;Margo WOJCIECHOWSKI;;;;;;;;;;;\nARRIERES;;;;;;;;;;;;;\n#9;1;Angélique BLANCHET;;;;;;;;;;;\n;2;Pauline BLONDIN;;;;;;;;;;;\n;3;Eugénie LEFRANC;;;;;;;;;;;\n#10;1;Laurine BRESSON;;;;;;;;;;;\n;2;Margot BUFFET;;;;;;;;;;;\n;3;Eileen LHOMME;;;;;;;;;;;\n3/4 CENTRES;1;Alix CHAMBON;;;;;;;;;;;\n;2;Maxine FOURDRIN;;;;;;;;;;;\n;3;Anna GANIERE;;;;;;;;;;;\nAILIERES;1;Mélyssa GROS;;;;;;;;;;;\n;2;Cerise NEURY;;;;;;;;;;;\n;3;Clémence ROUSSELET;;;;;;;;;;;\n;4;Fabyola BROSSET;;;;;;;;;;;\n;;;;;;;;;;;;;\nNIVEAU MOYEN;;;;;;;;;;;;;\nAVANTS;;;;;;;;;;;;;\nPILIERS;1;Priscilla AMOUSSOU;;;;;;;;;;;\n;2;Alexandra BONNET;;;;;;;;;;;\n;3;Eva DAUPHIN;;;;;;;;;;;\n;4;Pauline DAUZAT;;;;;;;;;;;\n;5;Elise LEVEQUE;;;;;;;;;;;\nARRIERES;;;;;;;;;;;;;\n#9;1;Chloé BELORGEOT;;;;;;;;;;;\n;2;Charlotte BETTENCOURT;;;;;;;;;;;\n;3;Astrid BOUCARD;;;;;;;;;;;\n;4;Louane CHABANAT;;;;;;;;;;;\n;5;Chloé CHABRIER;;;;;;;;;;;\n#10;6;Emma CHAUCHARD;;;;;;;;;;;\n;7;Louna CHAVANON;;;;;;;;;;;\n;8;Juliette DALIBARD;;;;;;;;;;;\n;9;Clémence RIEUMAIHOL;;;;;;;;;;;"))
                    )
                , h1 [] [ text "Ci-dessous, le fichier excel et la façon dont il faut l'enregistrer" ]
                , img [ src "img/marche_a_suivre_team_shuffler.png" ] []
                ]

          else
            div [] (Array.indexedMap view_bucket model.buckets |> Array.toList)
        ]


view_bucket index jlist =
    div [] [ h4 [] [ text ("Équipe n°" ++ String.fromInt (index + 1)), ul [] (List.map view_joueuse jlist) ] ]


view_joueuse ( ( joueuse, _ ), groupe, position ) =
    div []
        [ li [] [ text (joueuse ++ " " ++ groupe ++ " " ++ position) ] ]


fileDecoder : Decoder (List File)
fileDecoder =
    Json.Decode.at [ "target", "files" ] (Json.Decode.list File.decoder)
