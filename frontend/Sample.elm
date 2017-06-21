module Sample exposing (..)

import Http
import Json.Decode as JD
import Json.Encode as JE
import Helpers as H
import Task
import Html exposing (Html, text, div, a, h2, h3, textarea)
import Markdown

import Rubric
import Api

type alias Entity =
    { id: Int
    , name: String
    , rubric_id: Int
    , preinvestigation: String
    , investigation: String
    , trial: String
    }

type Status = Saving | Saved | NotSaved

type alias Model =
    { list: List Entity
    , current: Maybe Entity
    , name: String
    , status: Status
    }

init : Model
init =
    { list = []
    , current = Nothing
    , name = ""
    , status = Saved
    }


type Msg
    = Add Rubric.Entity
    | Delete Entity
    | Save Entity
    | ChangeName String
    | LoadList (Result Http.Error (List Entity))
    | ChangeCurrent Entity
    | EntitySaved (Result Http.Error ())

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    ChangeName s -> { model | name = s} ! []
    Add r -> model ! [addEntity r model.name]
    Delete e -> { model | current = Nothing } ! [deleteEntity e]
    Save e -> { model | status = Saving } ! [saveEntity e]
    ChangeCurrent e -> { model | current = Just e } ! []
    LoadList (Ok ss) -> { model | list = ss } ! []
    LoadList (Err _) -> model ! []
    EntitySaved (Ok _) -> { model | status = Saved } ! []
    EntitySaved (Err _) -> { model | status = NotSaved } ! []

makeMenuItem : Entity -> H.MenuItem Msg
makeMenuItem e =
    { name = e.name
    , href = "#/" ++ toString e.rubric_id ++ "/" ++ toString e.id
    , onClick = ChangeCurrent e
    , onDelete = Delete e
    }

render : Model -> Bool -> Rubric.Entity -> Html Msg
render model adminMode rubric =
    let
        menu = H.renderMenu adminMode
            { label = "Условие"
            , val = model.name 
            , onAdd = Add rubric
            , onChange = ChangeName 
            }
            (List.map makeMenuItem model.list)
        status =
            case model.status of
                Saved -> ""
                Saving -> "Сохранение..."
                NotSaved -> "Не удалось сохранить"
        field title setter val =
            [ h3 [] [text title]
            , if adminMode
                then H.textField title (\x -> ChangeCurrent <| setter x) val
                else Markdown.toHtml [] val
            ]
        content = \s ->
            H.simpleForm <|
                [ h2 [] [text s.name] ]
                ++ field "---НЕТ---" (\x -> { s | preinvestigation = x }) s.preinvestigation
                ++ field "Ситуации предварительного следствия" (\x -> { s | investigation = x }) s.investigation
                ++ field "Ситуации судебного следствия" (\x -> { s | trial = x }) s.trial
                ++ [ H.renderIf adminMode <| H.submit "Сохранить" status (Save s) ]
    in 
        div []
            [ menu
            , H.renderJust model.current content
            ]


-- API ------------------------------------------------------------------------

listRequest : Int -> Http.Request (List Entity)
listRequest rId =
    Http.get
        (Api.url ++ "/sample?rubric_id=eq." ++ toString rId)
        (JD.list entityDecoder)

getList : Rubric.Entity -> Cmd Msg
getList r =
    Http.send LoadList <| listRequest r.id

addEntity : Rubric.Entity -> String -> Cmd Msg
addEntity r name =
    let
        body = JE.object [("name", JE.string name), ("rubric_id", JE.int r.id)]
        request = H.noDecodePost (Api.url ++ "/sample") (Http.jsonBody body)
    in
        (Http.toTask request)
            |> Task.andThen (\_ -> Http.toTask <| listRequest r.id)
            |> Task.attempt LoadList

deleteEntity : Entity -> Cmd Msg
deleteEntity e =
    let
        request = H.noDecodeDelete (Api.url ++ "/sample?id=eq." ++ toString e.id)
    in
        (Http.toTask request)
            |> Task.andThen (\_ -> Http.toTask <| listRequest e.rubric_id)
            |> Task.attempt LoadList

saveEntity : Entity -> Cmd Msg
saveEntity e =
    let
        request = H.noDecodePatch
            (Api.url ++ "/sample?id=eq." ++ toString e.id)
            (Http.jsonBody <| encodeEntity e)
    in
        Http.send EntitySaved request


-- JSON -----------------------------------------------------------------------

entityDecoder : JD.Decoder Entity
entityDecoder = JD.map6 Entity
    (JD.field "id" JD.int)
    (JD.field "name" JD.string)
    (JD.field "rubric_id" JD.int)
    (JD.field "preinvestigation" JD.string)
    (JD.field "investigation" JD.string)
    (JD.field "trial" JD.string)

encodeEntity : Entity -> JD.Value
encodeEntity e = JE.object
    [ ("id", JE.int e.id)
    , ("name", JE.string e.name)
    , ("rubric_id", JE.int e.rubric_id)
    , ("preinvestigation", JE.string e.preinvestigation)
    , ("investigation", JE.string e.investigation)
    , ("trial", JE.string e.trial)
    ]

