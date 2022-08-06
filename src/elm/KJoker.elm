port module KJoker exposing (..)

import Browser exposing (Document)
import Bulma.Bulma
import Form exposing (Form)
import Form.Field
import Form.Validate exposing (Validation, andMap, succeed)
import Html exposing (Html, button, div, h1, header, p, section, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Http
import Json.Decode exposing (Decoder, andThen, int)
import Json.Decode.Pipeline exposing (required)


type JokeContent
    = SingleJoke String
    | TwoPartJoke String String


type JokeType
    = Single
    | TwoPart
    | Unknown


type alias Joke =
    { metaData : JokeMetaData
    , content : JokeContent
    }


type alias JokeMetaData =
    { category : Category
    , flags : JokeFlag
    , id : Int
    , safe : Bool
    , lang : String
    }


jokeLanguages : List String
jokeLanguages =
    [ "de", "cz", "pt", "en", "es", "fr" ]


type Category
    = Programming
    | Misc
    | Dark
    | Pun
    | Spooky
    | Christmas


type alias JokeFlag =
    { nsfw : Bool
    , racist : Bool
    , sexist : Bool
    , religious : Bool
    , political : Bool
    , explicit : Bool
    }


type alias Model =
    { joke : Maybe Joke
    , showSettings : Bool
    , settings : Form () Settings
    }


type alias Settings =
    { lang : String
    , flags : FlagSettings
    , categories : CategorySettings
    , safe : Bool
    }


type alias FlagSettings =
    { nsfw : Bool
    , racist : Bool
    , sexist : Bool
    , religious : Bool
    , political : Bool
    , explicit : Bool
    }


type alias CategorySettings =
    { programming : Bool
    , misc : Bool
    , dark : Bool
    , pun : Bool
    , spooky : Bool
    , christmas : Bool
    }


type Msg
    = NoOp
    | JokeReceived (Result Http.Error Joke)
    | ToggleSettings
    | LoadJoke
    | FormMsg Form.Msg


type alias SpeechSynthesis =
    { lang : String
    , text : String
    }


init : flags -> ( Model, Cmd Msg )
init _ =
    ( { joke = Nothing
      , showSettings = False
      , settings =
            Form.initial
                [ ( "lang", Form.Field.string "de" )
                , ( "flags"
                  , Form.Field.group
                        [ ( "nsfw", Form.Field.bool False )
                        , ( "racist", Form.Field.bool False )
                        , ( "sexist", Form.Field.bool False )
                        , ( "religious", Form.Field.bool False )
                        , ( "political", Form.Field.bool False )
                        , ( "explicit", Form.Field.bool False )
                        ]
                  )
                , ( "categories"
                  , Form.Field.group
                        [ ( "Programming", Form.Field.bool True )
                        , ( "Misc", Form.Field.bool True )
                        , ( "Dark", Form.Field.bool True )
                        , ( "Pun", Form.Field.bool True )
                        , ( "Spooky", Form.Field.bool True )
                        , ( "Christmas", Form.Field.bool True )
                        ]
                  )
                , ( "safe", Form.Field.bool True )
                ]
                validateSettings
      }
    , Cmd.none
    )


validateSettings : Validation () Settings
validateSettings =
    succeed Settings
        |> andMap (Form.Validate.field "lang" Form.Validate.string)
        |> andMap (Form.Validate.field "flags" validateFlags)
        |> andMap (Form.Validate.field "categories" validateCategories)
        |> andMap (Form.Validate.field "safe" Form.Validate.bool)


validateFlags : Validation () FlagSettings
validateFlags =
    succeed FlagSettings
        |> andMap (Form.Validate.field "nsfw" Form.Validate.bool)
        |> andMap (Form.Validate.field "racist" Form.Validate.bool)
        |> andMap (Form.Validate.field "sexist" Form.Validate.bool)
        |> andMap (Form.Validate.field "religious" Form.Validate.bool)
        |> andMap (Form.Validate.field "political" Form.Validate.bool)
        |> andMap (Form.Validate.field "explicit" Form.Validate.bool)


validateCategories : Validation () CategorySettings
validateCategories =
    succeed CategorySettings
        |> andMap (Form.Validate.field "Programming" Form.Validate.bool)
        |> andMap (Form.Validate.field "Misc" Form.Validate.bool)
        |> andMap (Form.Validate.field "Dark" Form.Validate.bool)
        |> andMap (Form.Validate.field "Pun" Form.Validate.bool)
        |> andMap (Form.Validate.field "Spooky" Form.Validate.bool)
        |> andMap (Form.Validate.field "Christmas" Form.Validate.bool)


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        NoOp ->
            ( model, Cmd.none )

        LoadJoke ->
            ( { model | joke = Nothing }
            , case Form.getOutput model.settings of
                Just settings ->
                    loadJoke settings

                Nothing ->
                    Cmd.none
            )

        ToggleSettings ->
            ( { model | showSettings = not model.showSettings }, Cmd.none )

        FormMsg formMsg ->
            ( { model | settings = Form.update validateSettings formMsg model.settings }, Cmd.none )

        JokeReceived result ->
            case result of
                Ok joke ->
                    ( { model | joke = Just joke }, speechSynthesis <| jokeToSpeechSynthesis joke )

                Err _ ->
                    ( model, Cmd.none )


view : Model -> Document Msg
view model =
    { title = "Jokes"
    , body =
        [ case model.joke of
            Just joke ->
                div [ class "joke-container" ] [ jokeHtml joke ]

            Nothing ->
                text ""
        , button [ class "button", onClick LoadJoke ] [ text "Load Joke" ]
        , button [ class "button", onClick ToggleSettings ] [ text "Show Settings" ]
        , div
            [ class "modal"
            , if model.showSettings then
                class "is-active"

              else
                class ""
            ]
            [ div [ class "modal-background", onClick ToggleSettings ] []
            , div [ class "modal-card" ]
                [ header [ class "modal-card-head" ]
                    [ p [ class "modal-card-title" ] [ text "Settings" ]
                    , button [ class "delete", onClick ToggleSettings ] []
                    ]
                , section [ class "modal-card-body" ] [ Html.map FormMsg (settingsView model.settings) ]
                ]
            ]
        ]
    }


settingsView : Form () Settings -> Html Form.Msg
settingsView form =
    div []
        [ div [ class "box" ]
            [ h1 [ class "subtitle is-6" ] [ text "Language" ]
            , Bulma.Bulma.bulmaFormRadio "lang" (List.map (\l -> ( l, l )) jokeLanguages) (Form.getFieldAsString "lang" form)
            ]
        , div [ class "box" ]
            [ h1 [ class "subtitle is-6" ] [ text "Blacklisted flags" ]
            , Bulma.Bulma.bulmaFormCheckbox " nsfw" (Form.getFieldAsBool "flags.nsfw" form)
            , Bulma.Bulma.bulmaFormCheckbox " racist" (Form.getFieldAsBool "flags.racist" form)
            , Bulma.Bulma.bulmaFormCheckbox " sexist" (Form.getFieldAsBool "flags.sexist" form)
            , Bulma.Bulma.bulmaFormCheckbox " religious" (Form.getFieldAsBool "flags.religious" form)
            , Bulma.Bulma.bulmaFormCheckbox " political" (Form.getFieldAsBool "flags.political" form)
            , Bulma.Bulma.bulmaFormCheckbox " explicit" (Form.getFieldAsBool "flags.explicit" form)
            ]
        , div [ class "box" ]
            [ h1 [ class "subtitle is-6" ] [ text "Allowed categories" ]
            , Bulma.Bulma.bulmaFormCheckbox " Programming" (Form.getFieldAsBool "categories.Programming" form)
            , Bulma.Bulma.bulmaFormCheckbox " Misc" (Form.getFieldAsBool "categories.Misc" form)
            , Bulma.Bulma.bulmaFormCheckbox " Dark" (Form.getFieldAsBool "categories.Dark" form)
            , Bulma.Bulma.bulmaFormCheckbox " Pun" (Form.getFieldAsBool "categories.Pun" form)
            , Bulma.Bulma.bulmaFormCheckbox " Spooky" (Form.getFieldAsBool "categories.Spooky" form)
            , Bulma.Bulma.bulmaFormCheckbox " Christmas" (Form.getFieldAsBool "categories.Christmas" form)
            ]
        ]


jokeHtml : Joke -> Html Msg
jokeHtml joke =
    jokeContentHtml joke.content


jokeContentHtml : JokeContent -> Html Msg
jokeContentHtml jokeContent =
    case jokeContent of
        SingleJoke joke ->
            div [] [ text joke ]

        TwoPartJoke setup delivery ->
            div []
                [ div [] [ text setup ]
                , div [] [ text delivery ]
                ]


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


loadJoke : Settings -> Cmd Msg
loadJoke settings =
    Http.get { url = settingsToUrl settings, expect = Http.expectJson JokeReceived jokeDecoder }



-- Http.get { url = "/joke.json", expect = Http.expectJson JokeReceived jokeDecoder }


jokeDecoder : Decoder Joke
jokeDecoder =
    typeDecoder
        |> andThen
            (\t ->
                case t of
                    Single ->
                        Json.Decode.map2 Joke metaDataDecoder singleJokeDecoder

                    TwoPart ->
                        Json.Decode.map2 Joke metaDataDecoder twoPartJokeDecoder

                    Unknown ->
                        Json.Decode.fail "type should be single or twopart"
            )


singleJokeDecoder : Decoder JokeContent
singleJokeDecoder =
    Json.Decode.succeed SingleJoke
        |> required "joke" Json.Decode.string


twoPartJokeDecoder : Decoder JokeContent
twoPartJokeDecoder =
    Json.Decode.succeed TwoPartJoke
        |> required "setup" Json.Decode.string
        |> required "delivery" Json.Decode.string


metaDataDecoder : Decoder JokeMetaData
metaDataDecoder =
    Json.Decode.succeed JokeMetaData
        |> required "category" categoryDecoder
        |> required "flags" flagDecoder
        |> required "id" int
        |> required "safe" Json.Decode.bool
        |> required "lang" Json.Decode.string


flagDecoder : Decoder JokeFlag
flagDecoder =
    Json.Decode.succeed JokeFlag
        |> required "nsfw" Json.Decode.bool
        |> required "racist" Json.Decode.bool
        |> required "sexist" Json.Decode.bool
        |> required "religious" Json.Decode.bool
        |> required "political" Json.Decode.bool
        |> required "explicit" Json.Decode.bool


categoryDecoder : Decoder Category
categoryDecoder =
    Json.Decode.string
        |> Json.Decode.map stringToCategory


stringToCategory : String -> Category
stringToCategory s =
    case s of
        "Programming" ->
            Programming

        "Misc" ->
            Misc

        "Dark" ->
            Dark

        "Pun" ->
            Pun

        "Spooky" ->
            Spooky

        "Christmas" ->
            Christmas

        _ ->
            Misc


categoryToString : Category -> String
categoryToString c =
    case c of
        Programming ->
            "Programming"

        Misc ->
            "Misc"

        Dark ->
            "Dark"

        Pun ->
            "Pun"

        Spooky ->
            "Spooky"

        Christmas ->
            "Christmas"


typeDecoder : Decoder JokeType
typeDecoder =
    Json.Decode.field "type" Json.Decode.string
        |> andThen
            (\s ->
                case s of
                    "single" ->
                        Json.Decode.succeed Single

                    _ ->
                        Json.Decode.succeed TwoPart
            )


stringToType : String -> JokeType
stringToType s =
    case s of
        "single" ->
            Single

        "twopart" ->
            TwoPart

        _ ->
            Unknown


jokeToSpeechSynthesis : Joke -> SpeechSynthesis
jokeToSpeechSynthesis joke =
    case joke.content of
        SingleJoke text ->
            { lang = joke.metaData.lang, text = text }

        TwoPartJoke setup delivery ->
            { lang = joke.metaData.lang, text = setup ++ " ... " ++ delivery }


settingsToUrl : Settings -> String
settingsToUrl settings =
    "/.netlify/functions/joke" ++ queryPart settings


queryPart : Settings -> String
queryPart settings =
    "?"
        ++ (String.join "&" <|
                List.filterMap identity
                    [ Just <| "lang=" ++ settings.lang, categorySettingsToString settings.categories, flagSettingsToString settings.flags ]
           )


categorySettingsToString : CategorySettings -> Maybe String
categorySettingsToString categories =
    let
        categoryList : List ( String, Bool )
        categoryList =
            [ ( "Programming", categories.programming )
            , ( "Misc", categories.misc )
            , ( "Dark", categories.dark )
            , ( "Pun", categories.pun )
            , ( "Spooky", categories.spooky )
            , ( "Christmas", categories.christmas )
            ]

        activeCategories : List String
        activeCategories =
            List.filter (\( _, isActive ) -> isActive) categoryList
                |> List.map (\( cat, _ ) -> cat)

        allActive : Bool
        allActive =
            List.all (\( _, isActive ) -> isActive) categoryList
    in
    if List.isEmpty activeCategories || allActive then
        Nothing

    else
        Just <| "categories=" ++ String.join "," activeCategories


flagSettingsToString : FlagSettings -> Maybe String
flagSettingsToString flags =
    let
        flagList : List ( String, Bool )
        flagList =
            [ ( "explicit", flags.explicit )
            , ( "nsfw", flags.nsfw )
            , ( "political", flags.political )
            , ( "racist", flags.racist )
            , ( "religious", flags.religious )
            , ( "sexist", flags.sexist )
            ]

        activeFlags : List String
        activeFlags =
            List.filter (\( _, isActive ) -> isActive) flagList
                |> List.map (\( cat, _ ) -> cat)
    in
    if List.isEmpty activeFlags then
        Nothing

    else
        Just <| "flags=" ++ String.join "," activeFlags


port speechSynthesis : SpeechSynthesis -> Cmd msg
