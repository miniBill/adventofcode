module FileProgram exposing (fileMain)

import Browser
import File exposing (File)
import File.Select as Select
import Html as H exposing (Attribute, Html)
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode as D
import Task



-- MAIN


fileMain : (String -> Html Never) -> Program () Model Msg
fileMain innerView =
    Browser.element
        { init = init
        , view = view innerView
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { hover : Bool
    , files : List String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model False [], Cmd.none )



-- UPDATE


type Msg
    = Pick
    | DragEnter
    | DragLeave
    | GotFiles File (List File)
    | ReadFiles (List String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Pick ->
            ( model
            , Select.files [ "text/*" ] GotFiles
            )

        DragEnter ->
            ( { model | hover = True }
            , Cmd.none
            )

        DragLeave ->
            ( { model | hover = False }
            , Cmd.none
            )

        GotFiles file files ->
            ( { model | hover = False }
            , Task.perform ReadFiles <| Task.sequence (List.map File.toString <| file :: files)
            )

        ReadFiles files ->
            ( { model | files = files }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : (String -> Html Never) -> Model -> Html Msg
view innerView model =
    case model.files of
        [ file ] ->
            H.map never <| innerView file

        _ ->
            H.div
                [ HA.style "border"
                    (if model.hover then
                        "6px dashed purple"

                     else
                        "6px dashed #ccc"
                    )
                , HA.style "border-radius" "20px"
                , HA.style "width" "480px"
                , HA.style "height" "100px"
                , HA.style "margin" "100px auto"
                , HA.style "padding" "20px"
                , HA.style "display" "flex"
                , HA.style "flex-direction" "column"
                , HA.style "justify-content" "center"
                , HA.style "align-items" "center"
                , hijackOn "dragenter" (D.succeed DragEnter)
                , hijackOn "dragover" (D.succeed DragEnter)
                , hijackOn "dragleave" (D.succeed DragLeave)
                , hijackOn "drop" dropDecoder
                ]
                [ H.button [ HE.onClick Pick ]
                    [ H.text <|
                        if List.length model.files == 0 then
                            "Upload Input"

                        else
                            "Upload a single file only!"
                    ]
                ]


dropDecoder : D.Decoder Msg
dropDecoder =
    D.at [ "dataTransfer", "files" ] (D.oneOrMore GotFiles File.decoder)


hijackOn : String -> D.Decoder msg -> Attribute msg
hijackOn event decoder =
    HE.preventDefaultOn event (D.map hijack decoder)


hijack : msg -> ( msg, Bool )
hijack msg =
    ( msg, True )
