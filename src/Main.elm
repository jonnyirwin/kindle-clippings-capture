module Main exposing (..)

import Browser
import File exposing (File)
import File.Select as Select
import Html exposing (Attribute, Html, button, div, h1, input, label, p, span, text)
import Html.Attributes exposing (attribute, class, for, id, name, style)
import Html.Events exposing (onClick, preventDefaultOn)
import Json.Decode exposing (Decoder, at, map, oneOrMore, succeed)
import Svg exposing (path, svg)
import Svg.Attributes exposing (d, fill, stroke, strokeLinecap, strokeLinejoin, strokeWidth, viewBox)
import Task


type alias Highlight =
    { bookTitle : String
    , author : Maybe String
    , location : Maybe String
    , highlightContent : String
    , note : Maybe String
    }



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { clippings : Maybe String
    , hover : Bool
    }


initialModel : Model
initialModel =
    { clippings = Nothing
    , hover = False
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel, Cmd.none )



-- UPDATE


type Msg
    = ClippingsRequested
    | ClippingsSelected File (List File)
    | ClippingsLoaded String
    | ClearClippings
    | DragEnter
    | DragLeave


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClippingsRequested ->
            ( model
            , Select.files [ "text/plain" ] ClippingsSelected
            )

        ClippingsSelected file files ->
            ( model
            , Task.perform ClippingsLoaded (File.toString file)
            )

        ClippingsLoaded content ->
            ( { model | clippings = Just content }
            , Cmd.none
            )

        ClearClippings ->
            ( { model | clippings = Nothing, hover = False }, Cmd.none )

        DragEnter ->
            ( { model | hover = True }, Cmd.none )

        DragLeave ->
            ( { model | hover = False }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    case model.clippings of
        Nothing ->
            div [ class "mt-1 sm:mt-0 sm:col-span-2" ]
                [ div
                    [ class
                        (if model.hover then
                            "border-4 border-gray-900"

                         else
                            "border-2 border-gray-300"
                        )
                    , class "max-w-lg flex justify-center mx-auto px-6 pt-5 pb-6 border-dashed rounded-md"
                    , hijackOn "drop" dropDecoder
                    , hijackOn "dragenter" (succeed DragEnter)
                    , hijackOn "dragover" (succeed DragEnter)
                    , hijackOn "dragleave" (succeed DragLeave)
                    ]
                    [ div [ class "space-y-1 text-center" ]
                        [ svg [ Svg.Attributes.class "mx-auto h-12 w-12 text-gray-400", stroke "currentColor", fill "none", viewBox "0 0 48 48", attribute "aria-hidden" "true" ]
                            [ path [ d "M28 8H12a4 4 0 00-4 4v20m32-12v8m0 0v8a4 4 0 01-4 4H12a4 4 0 01-4-4v-4m32-4l-3.172-3.172a4 4 0 00-5.656 0L28 28M8 32l9.172-9.172a4 4 0 015.656 0L28 28m0 0l4 4m4-24h8m-4-4v8m-12 4h.02", strokeWidth "2", strokeLinecap "round", strokeLinejoin "round" ] []
                            ]
                        , div [ class "flex text-sm text-gray-600" ]
                            [ button [ onClick ClippingsRequested ] [ text "Upload a file" ]
                            , p [ class "pl-1" ] [ text "or drag and drop" ]
                            ]
                        , p [ class "text-xs text-gray-500" ] [ text "My-Clippings.txt up to 10MB" ]
                        ]
                    ]
                ]

        Just content ->
            div []
                [ button [ onClick ClearClippings, class "inline-flex items-center px-3 py-2 border border-transparent text-sm leading-4 font-medium rounded-md shadow-sm text-white bg-indigo-600 hover:bg-indigo-700 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-indigo-500" ] [ text "Reset" ]
                , p [ style "white-space" "pre" ] [ text content ]
                ]


dropDecoder : Decoder Msg
dropDecoder =
    at [ "dataTransfer", "files" ] (oneOrMore ClippingsSelected File.decoder)


hijackOn : String -> Decoder msg -> Attribute msg
hijackOn event decoder =
    preventDefaultOn event (map hijack decoder)


hijack : msg -> ( msg, Bool )
hijack msg =
    ( msg, True )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
