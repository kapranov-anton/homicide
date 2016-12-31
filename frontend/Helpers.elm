module Helpers exposing (..)

import Http
import Html exposing (Html, Attribute, div, textarea, button, text, ul, li)
import Html.Attributes exposing (placeholder, value)
import Html.Events exposing (onClick, onInput, on, keyCode)
import Json.Decode as JD


noDecodeRequest : String -> String -> Http.Body -> Http.Request ()
noDecodeRequest method url body =
    Http.request
        { method = method
        , headers = []
        , url = url
        , body = body
        , expect = Http.expectStringResponse (\_ -> Ok ())
        , timeout = Nothing
        , withCredentials = False
        }

noDecodePost : String -> Http.Body -> Http.Request ()
noDecodePost = noDecodeRequest "POST"

noDecodePatch : String -> Http.Body -> Http.Request ()
noDecodePatch = noDecodeRequest "PATCH"

noDecodeDelete : String -> Http.Request ()
noDecodeDelete url = noDecodeRequest "DELETE" url Http.emptyBody

emptyNode : Html msg
emptyNode = div [] []

renderIf : Bool -> Html msg -> Html msg
renderIf cond node = if cond then node else emptyNode

renderJust : Maybe x -> (x -> Html msg) -> Html msg
renderJust maybeVal render =
    Maybe.map render maybeVal
        |> Maybe.withDefault emptyNode

addForm : String -> msg -> (String -> msg) -> String -> Html msg
addForm val addHandler inputHandler title =
    div []
        [ div []
            [textarea [placeholder title, value val, onInput inputHandler, onEnter addHandler] []]
        , div []
            [button [onClick addHandler] [text "Добавить"]]
        ]

deleteButton : msg -> Html msg
deleteButton deleteHandler =
    div []
        [button [onClick deleteHandler] [text "Удалить"]]

renderList : (a -> Html msg) -> List a -> Html msg
renderList renderItem items = ul [] <| List.map (\i -> li [] [renderItem i]) items

onEnter : msg -> Attribute msg
onEnter msg =
    let
        isEnter code = case code of
            13 -> JD.succeed msg
            _ -> JD.fail "not enter"
    in
        on "keydown" (JD.andThen isEnter keyCode)

