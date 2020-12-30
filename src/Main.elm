port module Main exposing (main)

import Browser exposing (element)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input exposing (button)
import Firestore
import Firestore.Config as Config
import Firestore.Decode as FSDecode
import Firestore.Encode as FSEncode
import Html exposing (Html, div)
import Html.Attributes exposing (attribute, class, property, style)
import Http exposing (Error(..))
import Json.Decode as Decode
import Json.Decode.Pipeline as D
import Json.Encode as JS
import Task
import Time exposing (Month(..), Posix, toDay, toMonth, toYear, utc)



---- PORTS ----


port signIn : () -> Cmd msg


port signInInfo : (JS.Value -> msg) -> Sub msg


port signInError : (JS.Value -> msg) -> Sub msg


port signOut : () -> Cmd msg



---- MODEL ----


type alias Routine =
    { date : Posix
    , times : Int
    }


type alias User =
    { token : String
    , email : String
    , uid : String
    }


type alias ErrorData =
    { code : String
    , message : String
    , credential : String
    }


type State
    = LoggedOut
    | LoggedIn
    | Loading
    | Error String


type alias Model =
    { config : Config.Config
    , firestore : Firestore.Firestore
    , routines : List (Firestore.Document Routine)
    , selectedDate : Maybe Posix
    , state : State
    , user : String
    }


type alias Flags =
    ( String, String )


initialState : Config.Config -> Maybe Posix -> Model
initialState config time =
    { config = config
    , firestore = Firestore.init config
    , routines = []
    , selectedDate = time
    , state = LoggedOut
    , user = ""
    }


init : Flags -> ( Model, Cmd Msg )
init ( apiKey, project ) =
    ( initialState (Config.new { apiKey = apiKey, project = project }) Nothing
    , Task.perform (Just >> SetTime) Time.now
    )


listConfig : Maybe Firestore.PageToken -> Firestore.ListOption
listConfig token =
    { pageSize = 300, orderBy = "date", pageToken = token }



---- UPDATE ----


type Msg
    = LogIn
    | LogOut
    | LoggedInData (Result Decode.Error User)
    | LoggedInError (Result Decode.Error ErrorData)
    | SetTime (Maybe Posix)
    | RoutineCompleted Posix
    | RecordRoutine (Result Firestore.Error (Firestore.Document Routine))
    | FetchRoutines (Result Firestore.Error (Firestore.Documents Routine))


callFirestore : Model -> Maybe Firestore.PageToken -> ( Model, Cmd Msg )
callFirestore model token =
    ( model
    , model.firestore
        |> Firestore.list decoder (listConfig token)
        |> Task.attempt FetchRoutines
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LogIn ->
            ( model, signIn () )

        LogOut ->
            ( initialState model.config model.selectedDate, signOut () )

        SetTime time ->
            ( { model | selectedDate = time }, Cmd.none )

        RoutineCompleted date ->
            ( model
            , model.firestore
                |> Firestore.insert decoder (encoder date)
                |> Task.attempt RecordRoutine
            )

        LoggedInData (Err error) ->
            ( { model | state = Error <| Decode.errorToString error }, Cmd.none )

        LoggedInData (Ok { email, uid }) ->
            case model.state of
                LoggedOut ->
                    let
                        firestore =
                            model.firestore |> Firestore.withCollection ("users/" ++ uid ++ "/routines")
                    in
                    callFirestore { model | user = email, firestore = firestore, state = Loading } Nothing

                -- if for some reason we received another login (being logged in)... do nothing about it
                _ ->
                    ( model, Cmd.none )

        LoggedInError (Ok { message }) ->
            ( { model | state = Error message }, Cmd.none )

        LoggedInError (Err error) ->
            ( { model | state = Error <| Decode.errorToString error }, Cmd.none )

        RecordRoutine (Ok chapter) ->
            ( { model | routines = chapter :: model.routines }, Cmd.none )

        RecordRoutine (Err _) ->
            ( { model | state = Error "Could not record reading, try refreshing the page!" }, Cmd.none )

        FetchRoutines (Ok { documents, nextPageToken }) ->
            case nextPageToken of
                Nothing ->
                    case List.head documents of
                        Nothing ->
                            ( { model | state = LoggedIn }, Cmd.none )

                        Just _ ->
                            ( { model
                                | state = LoggedIn
                                , routines = model.routines ++ documents
                              }
                            , Cmd.none
                            )

                Just token ->
                    callFirestore { model | routines = model.routines ++ documents } <| Just token

        FetchRoutines (Err (Firestore.Http_ (BadUrl url))) ->
            ( { model | state = Error <| "The URL " ++ url ++ " was invalid" }, Cmd.none )

        FetchRoutines (Err (Firestore.Http_ Timeout)) ->
            ( { model | state = Error "Unable to reach the server, try again" }, Cmd.none )

        FetchRoutines (Err (Firestore.Http_ NetworkError)) ->
            ( { model | state = Error "Unable to reach the server, check your network connection" }, Cmd.none )

        FetchRoutines (Err (Firestore.Http_ (BadStatus code))) ->
            ( { model | state = Error <| "The server responded with BadStatus " ++ String.fromInt code }, Cmd.none )

        FetchRoutines (Err (Firestore.Http_ (BadBody _))) ->
            -- New users have do not have documents, but should be able to log in
            ( { model | state = LoggedIn }, Cmd.none )

        FetchRoutines (Err (Firestore.Response { message })) ->
            ( { model | state = Error message }, Cmd.none )


encoder : Posix -> FSEncode.Encoder
encoder date =
    FSEncode.document
        [ ( "date", FSEncode.timestamp date )
        , ( "times", FSEncode.int 1 )
        ]


decoder : FSDecode.Decoder Routine
decoder =
    FSDecode.document Routine
        |> FSDecode.required "date" FSDecode.timestamp
        |> FSDecode.required "times" FSDecode.int


userDataDecoder : Decode.Decoder User
userDataDecoder =
    Decode.succeed User
        |> D.required "token" Decode.string
        |> D.required "email" Decode.string
        |> D.required "uid" Decode.string


logInErrorDecoder : Decode.Decoder ErrorData
logInErrorDecoder =
    Decode.succeed ErrorData
        |> D.required "code" Decode.string
        |> D.required "message" Decode.string
        |> D.required "credential" Decode.string



---- VIEW ----


pink : Color
pink =
    rgb255 248 214 220


btns : List (Attribute msg)
btns =
    [ Background.color pink
    , Font.color <| rgb255 255 255 255
    , Border.rounded 3
    , padding 10
    , focused [ Background.color <| rgb255 186 147 154 ]
    , htmlAttribute <| style "box-shadow" "none"
    ]


small : String -> Element msg
small user =
    el [ Font.color <| rgb255 0 0 0, Font.size 16 ] <| text user


logOut : Element Msg
logOut =
    link
        [ Font.color pink
        , Font.underline
        , Font.size 16
        , Events.onClick LogOut
        ]
    <|
        { label = text "Log Out", url = "#" }


viewSelectors : Model -> Element Msg -> Element Msg
viewSelectors { selectedDate, user } node =
    column [ width fill, centerY, spacing 30 ]
        [ row [ spacing 10, centerX ]
            [ logOut
            , small user
            , button btns
                { label = text "YES!"
                , onPress = Maybe.map RoutineCompleted selectedDate
                }
            ]
        , el [ centerX ] node
        ]


view : Model -> Html Msg
view ({ routines, state } as model) =
    layout [] <|
        case state of
            Error message ->
                el [ centerY, centerX, Font.color <| rgb255 255 0 0 ] <|
                    text message

            LoggedOut ->
                el [ centerY, centerX ] <|
                    button btns { label = text "GOOGLE SIGN IN", onPress = Just LogIn }

            Loading ->
                viewSelectors model <|
                    html <|
                        div [ class "spinner" ] [ div [] [], div [] [], div [] [], div [] [] ]

            LoggedIn ->
                viewSelectors model <|
                    html <|
                        Html.node
                            "google-calendar"
                            [ attribute "type" "calendar"
                            , property "options" settings
                            , property "cols" <|
                                JS.list JS.object
                                    [ [ ( "id", JS.string "Date" ), ( "type", JS.string "date" ) ]
                                    , [ ( "id", JS.string "Routines" ), ( "type", JS.string "number" ) ]
                                    , [ ( "role", JS.string "tooltip" ), ( "type", JS.string "string" ) ]
                                    ]
                            , property "dateRows" <|
                                JS.list (JS.list identity)
                                    (routines
                                        |> List.map
                                            (\x ->
                                                [ JS.string <| formatDate x.fields.date
                                                , JS.int <| x.fields.times
                                                ]
                                            )
                                    )
                            ]
                            []



---- CALENDAR ----


grey : String
grey =
    "#eee"


formatDate : Posix -> String
formatDate date =
    String.join "-"
        [ String.fromInt <| toYear utc date
        , monthToString <| toMonth utc date
        , String.padLeft 2 '0' <| String.fromInt <| toDay utc date
        ]


monthToString : Month -> String
monthToString month =
    case month of
        Jan ->
            "01"

        Feb ->
            "02"

        Mar ->
            "03"

        Apr ->
            "04"

        May ->
            "05"

        Jun ->
            "06"

        Jul ->
            "07"

        Aug ->
            "08"

        Sep ->
            "09"

        Oct ->
            "10"

        Nov ->
            "11"

        Dec ->
            "12"


settings : JS.Value
settings =
    JS.object
        [ ( "title", JS.string "Have you taken care of yourself today?" )
        , ( "height", JS.int 460 )
        , ( "colorAxis"
          , JS.object
                [ ( "minValue", JS.int 0 )
                , ( "colors"
                  , JS.list JS.string [ grey, "#f8d6dc" ]
                  )
                ]
          )
        , ( "noDataPattern"
          , JS.object
                [ ( "backgroundColor", JS.string grey )
                , ( "color", JS.string grey )
                ]
          )
        , ( "calendar"
          , JS.object
                [ ( "daysOfWeek", JS.string " M W F" )
                , ( "cellColor", JS.object [ ( "strokeWidth", JS.float 2.5 ) ] )
                , ( "monthOutlineColor", JS.object [ ( "strokeOpacity", JS.int 0 ) ] )
                , ( "unusedMonthOutlineColor", JS.object [ ( "strokeOpacity", JS.int 0 ) ] )
                , ( "dayOfWeekLabel"
                  , JS.object
                        [ ( "fontName", JS.string "Helvetica" )
                        , ( "opacity", JS.float 0.5 )
                        , ( "fontSize", JS.int 12 )
                        , ( "color", JS.string "#aaa" )
                        ]
                  )
                , ( "monthLabel"
                  , JS.object
                        [ ( "fontName", JS.string "Helvetica" )
                        , ( "opacity", JS.float 0.5 )
                        , ( "fontSize", JS.int 12 )
                        ]
                  )
                ]
          )
        ]



---- PROGRAM ----


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ signInInfo (Decode.decodeValue userDataDecoder >> LoggedInData)
        , signInError (Decode.decodeValue logInErrorDecoder >> LoggedInError)
        ]


main : Program Flags Model Msg
main =
    element { view = view, init = init, update = update, subscriptions = subscriptions }
