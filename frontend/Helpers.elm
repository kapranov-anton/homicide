module Helpers exposing (..)

import Http
import Html exposing
    ( Html
    , Attribute
    , text
    , div, span
    , fieldset, textarea, button
    , ul, li
    , a
    , h1, h2, h3, h4
    )
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

textField : String -> (String -> msg) -> String -> Html msg
textField title inputHandler val =
    div []
        [ textarea [placeholder title, value val, onInput inputHandler] []
        ]

submit : String -> String -> msg -> Html msg
submit title comment handler =
    div []
        [ button
            [onClick handler, class "pure-button", class "pure-button-primary"]
            [text title]
        , span [class "submit-comment"] [text comment]
        ]

simpleForm : List (Html msg) -> Html msg
simpleForm fields =
    div [class "pure-form"]
        [ fieldset [class "pure-group"] fields ]

addForm : String -> msg -> (String -> msg) -> String -> Html msg
addForm val addHandler inputHandler title =
    simpleForm
        [ textField title inputHandler val
        , submit "Добавить" "" addHandler
        ]

deleteButton : msg -> Html msg
deleteButton deleteHandler =
    button
        [ onClick deleteHandler
        , class "pure-button"
        , class "left-float-button"
        ]
        [ text "Удалить" ]

type alias MenuItem msg =
    { name: String
    , href: String
    , onClick: msg
    , onDelete: msg
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

type alias MenuParams msg =
    { label: String
    , val: String
    , onAdd: msg
    , onChange: (String -> msg)
    }

renderMenu : Bool -> MenuParams msg -> List (MenuItem msg) -> Html msg
renderMenu adminMode params items =
    let
        renderListItem i = li [class "pure-menu-item"] [renderMenuItem adminMode i]
        renderedItems =
            ul
                [ class "pure-menu-list"
                , class "menu-list"
                ] <| List.map renderListItem items
        form = addForm params.val params.onAdd params.onChange params.label
    in
        div []
            [ renderedItems
            , renderIf adminMode <| form
            ]
        

onEnter : msg -> Attribute msg
onEnter msg =
    let
        isEnter code = case code of
            13 -> JD.succeed msg
            _ -> JD.fail "not enter"
    in
        on "keydown" (JD.andThen isEnter keyCode)

