module Rubric exposing (..)

import Http
import Json.Decode as JD
import Json.Encode as JE
import Helpers exposing
    ( MenuItem
    , noDecodePost
    , noDecodeDelete
    , renderIf
    , renderMenu
    , addForm
    , deleteButton
    )
import Task
import Html exposing (Html, div, text, a, h1)
import Html.Attributes exposing (href, class)
import Html.Events exposing (onClick)

import Api

type alias Model =
    { list: List Entity
    , current: Maybe Entity
    , name: String
    }

type alias Entity =
    { id: Int
    , name: String
    }

init : Model
init =
    { list = []
    , current = Nothing
    , name = ""
    }

type Msg
    = Reset
    | Add
    | Delete Entity
    | ChangeName String
    | LoadList (Result Http.Error (List Entity))
    | ChangeCurrent Entity

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Reset -> { model | current = Nothing } ! []
    Add -> model ! [addEntity model.name]
    Delete e -> { model | current = Nothing } ! [deleteEntity e]
    ChangeName r -> { model | name = r} ! []
    ChangeCurrent r -> { model | current = Just r } ! []
    LoadList (Ok rs) -> { model | list = rs } ! []
    LoadList (Err x) -> Debug.log "load list error" model ! []

entityDecoder : JD.Decoder Entity
entityDecoder = JD.map2 Entity
    (JD.field "id" JD.int)
    (JD.field "name" JD.string)

listRequest : Http.Request (List Entity)
listRequest =
    Http.get (Api.url ++ "/rubric") (JD.list entityDecoder)

getList : Cmd Msg
getList =
    Http.send LoadList listRequest

addEntity : String -> Cmd Msg
addEntity name =
    let
        body = JE.object [("name", JE.string name)]
        request = noDecodePost (Api.url ++ "/rubric") (Http.jsonBody body)
    in
        (Http.toTask request)
            |> Task.andThen (\_ -> Http.toTask listRequest)
            |> Task.attempt LoadList

deleteEntity : Entity -> Cmd Msg
deleteEntity e =
    let
        request = noDecodeDelete
            (Api.url ++ "/rubric?id=eq." ++ toString e.id)
    in
        (Http.toTask request)
            |> Task.andThen (\_ -> Http.toTask <| listRequest)
            |> Task.attempt LoadList

makeMenuItem : Entity -> MenuItem Msg
makeMenuItem e =
    { name = e.name
    , href = "#/" ++ toString e.id
    , onClick = ChangeCurrent e
    , onDelete = Delete e
    }

backLink : Html Msg
backLink =
    div [class "back-link"]
        [ text "← "
        , a [href "#/", onClick Reset] [text "К выбору рубрики"]
        ]

render : Model -> Bool -> Html Msg
render model adminMode =
    case model.current of
        Just r ->
            div []
                [ h1 [] [text r.name]
                , backLink
                ]
        Nothing ->
            div []
                [ renderMenu adminMode <| List.map makeMenuItem model.list
                , renderIf adminMode <| addForm model.name Add ChangeName "Рубрика"
                ]

