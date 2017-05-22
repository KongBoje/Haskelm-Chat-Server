import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode exposing (Decoder, string, at)
import Json.Decode as Decode
import Json.Encode as Encode
import WebSocket
import List

main: Program Never Model Msg
main =
  Html.program
  { init = init
  , view = view
  , update = update
  , subscriptions = subscriptions
  }

 -- MODEL
type alias Model =
  { chatMessage : List String
  , userMessage : String
  , loginName : String
  }

init : (Model, Cmd Msg)
init =
  ( Model [""] "" ""
  , Cmd.none
  )

type alias ChatMessage =
  { command: String
  , content: String
  }

-- UPDATE
type Msg
  = PostChatMessage
  | UpdateUserMessage String
  | NewChatMessage String
  | LoginMessage String

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    PostChatMessage ->
      let
        message = model.userMessage
      in
        { model | userMessage = "" } ! [WebSocket.send "ws://localhost:3000/" message]

    UpdateUserMessage message ->
      { model | userMessage = message } ! []

    NewChatMessage message ->
        { model | chatMessage = message :: model.chatMessage } ! []

    LoginMessage message ->
      {model | loginName = message} ! []

jsonToString : Result String String -> String
jsonToString result =
  case result of
    Ok result -> result
    Err result -> "Error"

-- VIEW
view : Model -> Html Msg
view model =
  div []
    [ h1 [] [ text ("Haskelm Chat Server!")]
    , input [ placeholder "message..."
            , autofocus True
            , value model.userMessage
            , onInput UpdateUserMessage
            ] []
    , button [ onClick PostChatMessage ] [ text "Submit" ]
    , h3 [] [ text ("Chat messages:")]
    , div [] (List.map showMessage model.chatMessage)
    ]

showMessage : String -> Html msg
showMessage message =
  div [] [text message]

 -- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model =
  WebSocket.listen "ws://localhost:3000" NewChatMessage
