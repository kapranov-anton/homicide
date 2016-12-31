module Rubric exposing (..)

import Http
import Json.Decode as JD
import Json.Encode as JE
import Helpers exposing
    ( noDecodePost
    , noDecodeDelete
    , renderIf
    , renderList
    , addForm
    , deleteButton
    )
import Task
import Html exposing (Html, div, text, a)
import Html.Attributes exposing (href)
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

renderEntity : Bool -> Entity -> Html Msg
renderEntity adminMode r = div []
    [ a [href ("#/" ++ toString r.id), onClick (ChangeCurrent r)]
        [text r.name]
    , renderIf adminMode <| deleteButton (Delete r)
    ]

render : Model -> Bool -> Html Msg
render model adminMode =
    case model.current of
        Just r ->
            div []
                [ a [href "#/", onClick Reset]
                    [text "Назад"]
                , text r.name
                ]
        Nothing ->
            div []
                [ renderList (renderEntity adminMode) model.list
                , renderIf adminMode <| addForm model.name Add ChangeName "Рубрика"
                ]

