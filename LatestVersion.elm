module LatestVersion exposing (..)

import Html exposing (Html, button, div, input, text, ul, li, span, program)
import Html.Events exposing (onClick, onInput)
import Http exposing (..)
import Json.Decode exposing (..)


-- Model


type alias Model =
    { searchTerm : String
    , results : List SearchResult
    }


type alias SearchResult =
    { groupId : String
    , artifactId : String
    , version : String
    }


init : ( Model, Cmd Msg )
init =
    ( { searchTerm = "", results = [] }, Cmd.none )


type Msg
    = ChangeSearchTerm String
    | PerformSearch
    | SearchComplete (Result Http.Error (List SearchResult))



-- View


view : Model -> Html Msg
view model =
    div []
        [ input [ onInput ChangeSearchTerm ] []
        , button [ onClick PerformSearch ] [ text "Search" ]
        , ul [] (List.map viewSearchResult model.results)
        ]


viewSearchResult : SearchResult -> Html Msg
viewSearchResult result =
    li []
        [ span [] [ text (toString result.version) ]
        ]



-- update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeSearchTerm newTerm ->
            ( { model | searchTerm = newTerm }, Cmd.none )

        PerformSearch ->
            ( model, issueSearch model.searchTerm )

        SearchComplete (Ok results) ->
            ( { model | results = results }, Cmd.none )

        SearchComplete (Err _) ->
            ( model, Cmd.none )



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- HTTP


issueSearch : String -> Cmd Msg
issueSearch criteria =
    let
        url =
            "http://nexus.containerstore.com/nexus/service/local/lucene/search?a="
                ++ criteria

        request =
            { verb = "GET"
            , headers = [ ( "Accept", "application/json" ) ]
            , url = url
            , body = ""
            }
    in
        Http.send SearchComplete (Http.get url decodeResults)


decodeResult : Decoder SearchResult
decodeResult =
    map3 SearchResult
        (field "groupId" string)
        (field "artifactId" string)
        (field "version" string)


decodeResults : Decoder (List SearchResult)
decodeResults =
    list decodeResult



-- Main


main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
