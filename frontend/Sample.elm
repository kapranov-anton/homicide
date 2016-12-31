module Sample exposing (..)

import Http
import Json.Decode as JD
import Json.Encode as JE
import Helpers exposing (noDecodePost, noDecodePatch, renderIf, addForm, renderList)
import Task
import Html exposing (Html, text, div, a)
import Html.Attributes exposing (href)
import Html.Events exposing (onClick)

import Rubric
import Api

type ListField = Preinvestigation | Investigation | Trial

type alias ListFieldRecord =
    { preinvestigation: String
    , investigation: String
    , trial: String
    }

getField : ListFieldRecord -> ListField -> String
getField rec lf = case lf of
    Preinvestigation -> rec.preinvestigation
    Investigation -> rec.investigation
    Trial -> rec.trial

getFieldLabel : ListField -> String
getFieldLabel lf = case lf of
    Preinvestigation -> "До расследования"
    Investigation -> "Во время расследования"
    Trial -> "Во время суда"

updateField : ListFieldRecord -> ListField -> String -> ListFieldRecord
updateField rec lf val= case lf of
    Preinvestigation -> { rec | preinvestigation = val }
    Investigation -> { rec | investigation = val }
    Trial -> { rec | trial = val }

type alias Model =
    { list: List Entity
    , current: Maybe Entity
    , name: String
    , listFields: ListFieldRecord
    }

type alias Entity =
    { id: Int
    , name: String
    , rubric_id: Int
    , preinvestigation: List String
    , investigation: List String
    , trial: List String
    }

getEntityField : Entity -> ListField -> List String
getEntityField e lf = case lf of
    Preinvestigation -> e.preinvestigation
    Investigation -> e.investigation
    Trial -> e.trial

updateEntityField : Entity -> ListField -> List String -> Entity
updateEntityField e lf val = case lf of
    Preinvestigation -> { e | preinvestigation = val }
    Investigation -> { e | investigation = val }
    Trial -> { e | trial = val }

init : Model
init =
    { list = []
    , current = Nothing
    , name = ""
    , listFields =
        { preinvestigation = ""
        , investigation = ""
        , trial = ""
        }
    }


type Msg
    = Add Rubric.Entity
    | ChangeName String
    | LoadList (Result Http.Error (List Entity))
    | ChangeCurrent Entity
    | ChangeListField ListField String
    | AddToList ListField Entity
    | EntitySaved ListField (Result Http.Error ())

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    ChangeName s -> { model | name = s} ! []
    Add r -> model ! [addEntity r model.name]
    ChangeCurrent e -> { model | current = Just e } ! []
    ChangeListField listField s ->
        { model | listFields = updateField model.listFields listField s } ! []
    AddToList listField e ->
        let
            value = getField model.listFields listField :: getEntityField e listField
            newCurrent = updateEntityField e listField value
        in
            { model | current = Just newCurrent} ! [saveEntity listField newCurrent]
    LoadList (Ok ss) -> { model | list = ss } ! []
    LoadList (Err _) -> model ! []
    EntitySaved listField (Ok _) ->
        { model | listFields = updateField model.listFields listField "" } ! []
    EntitySaved listField (Err _) ->
        model ! []

entityDecoder : JD.Decoder Entity
entityDecoder = JD.map6 Entity
    (JD.field "id" JD.int)
    (JD.field "name" JD.string)
    (JD.field "rubric_id" JD.int)
    (JD.field "preinvestigation" (JD.list JD.string))
    (JD.field "investigation" (JD.list JD.string))
    (JD.field "trial" (JD.list JD.string))

encodeEntity : Entity -> JD.Value
encodeEntity e = JE.object
    [ ("id", JE.int e.id)
    , ("name", JE.string e.name)
    , ("rubric_id", JE.int e.rubric_id)
    , ("preinvestigation", JE.list (List.map JE.string e.preinvestigation))
    , ("investigation", JE.list (List.map JE.string e.investigation))
    , ("trial", JE.list (List.map JE.string e.trial))
    ]

listRequest : Rubric.Entity -> Http.Request (List Entity)
listRequest r =
    Http.get
        (Api.url ++ "/sample?rubric_id=eq." ++ toString r.id)
        (JD.list entityDecoder)

getList : Rubric.Entity -> Cmd Msg
getList r =
    Http.send LoadList <| listRequest r

addEntity : Rubric.Entity -> String -> Cmd Msg
addEntity r name =
    let
        body = JE.object [("name", JE.string name), ("rubric_id", JE.int r.id)]
        request = noDecodePost (Api.url ++ "/sample") (Http.jsonBody body)
    in
        (Http.toTask request)
            |> Task.andThen (\_ -> Http.toTask <| listRequest r)
            |> Task.attempt LoadList

saveEntity : ListField -> Entity -> Cmd Msg
saveEntity lf e =
    let
        request = noDecodePatch
            (Api.url ++ "/sample?id=eq." ++ toString e.id)
            (Http.jsonBody <| encodeEntity e)
    in
        Http.send (EntitySaved lf) request


renderListField : Bool -> ListFieldRecord -> Entity -> ListField -> Html Msg
renderListField adminMode rec e listField =
    let
        title = getFieldLabel listField
        items = getEntityField e listField
    in
        div []
            [ renderList text items
            , addForm (getField rec listField) (AddToList listField e) (ChangeListField listField) title
                |> renderIf adminMode
            ]

renderEntity : Entity -> Html Msg
renderEntity s = div []
    [ a [ href ("#/" ++ toString s.rubric_id ++ "/" ++ toString s.id)
        , onClick (ChangeCurrent s)]
        [text s.name]
    ]


render : Model -> Bool -> Rubric.Entity -> Html Msg
render model adminMode rubric =
    case model.current of
        Just s ->
            let renderLF = renderListField adminMode model.listFields s
            in div []
                [ renderList renderEntity model.list
                , text s.name
                , renderLF Preinvestigation
                , renderLF Investigation
                , renderLF Trial
                ]
        Nothing ->
            div []
                [ renderList renderEntity model.list
                , renderIf adminMode <| addForm model.name (Add rubric) ChangeName "Условие"
                ]

