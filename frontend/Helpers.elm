module Helpers exposing (..)

import Http
import Html exposing (Html, Attribute, div, textarea, button, text, ul, li, a)
import Html.Attributes exposing (placeholder, value, class, href, style)
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
    div [class "pure-form"]
        [ div []
            [textarea [placeholder title, value val, onInput inputHandler] []]
        , div []
            [button
                [onClick addHandler, class "pure-button", class "pure-button-primary"]
                [text "Добавить"]]
        ]

deleteButton : msg -> Html msg
deleteButton deleteHandler =
    button
        [ onClick deleteHandler
        , class "pure-button"
        , class "left-float-button"
        ]
        [ text "Удалить" ]

type alias MenuItem a =
    { name: String
    , href: String
    , onClick: a
    , onDelete: a
    }

renderMenuItem : Bool -> MenuItem msg -> Html msg
renderMenuItem adminMode item =
    div []
        [ renderIf adminMode <| deleteButton item.onDelete
        , a [ href item.href
            , onClick item.onClick
            , class "pure-menu-link"
            ]
            [ text item.name ]
        ]

renderMenu : Bool -> List (MenuItem msg) -> Html msg
renderMenu adminMode items =
    let
        renderListItem i = li [class "pure-menu-item"] [renderMenuItem adminMode i]
    in
        ul
            [ class "pure-menu-list"
            , class "menu-list"
            ] <| List.map renderListItem items

onEnter : msg -> Attribute msg
onEnter msg =
    let
        isEnter code = case code of
            13 -> JD.succeed msg
            _ -> JD.fail "not enter"
    in
        on "keydown" (JD.andThen isEnter keyCode)

