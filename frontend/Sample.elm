module Sample exposing (..)

import Http
import Json.Decode as JD
import Json.Encode as JE
import Helpers exposing
    ( MenuItem
    , noDecodePost
    , noDecodePatch
    , noDecodeDelete
    , renderIf
    , renderMenu
    , addForm
    , deleteButton
    )
import Task
import Html exposing (Html, text, div, a, h2, h3)
import Html.Attributes exposing (class)

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
    | Delete Entity
    | ChangeName String
    | LoadList (Result Http.Error (List Entity))
    | ChangeCurrent Entity
    | ChangeListField ListField String
    | AddToList ListField Entity
    | DeleteFromList ListField Entity String
    | EntitySaved ListField (Result Http.Error ())

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    ChangeName s -> { model | name = s} ! []
    Add r -> model ! [addEntity r model.name]
    Delete e -> { model | current = Nothing } ! [deleteEntity e]
    ChangeCurrent e -> { model | current = Just e } ! []
    ChangeListField listField s ->
        { model | listFields = updateField model.listFields listField s } ! []
    DeleteFromList listField e item ->
        let
            value = List.filter (\i -> i /= item) <| getEntityField e listField
            newCurrent = updateEntityField e listField value
        in
            { model | current = Just newCurrent} ! [saveEntity listField newCurrent]
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
        request = noDecodePost (Api.url ++ "/sample") (Http.jsonBody body)
    in
        (Http.toTask request)
            |> Task.andThen (\_ -> Http.toTask <| listRequest r.id)
            |> Task.attempt LoadList

deleteEntity : Entity -> Cmd Msg
deleteEntity e =
    let
        request = noDecodeDelete
            (Api.url ++ "/sample?id=eq." ++ toString e.id)
    in
        (Http.toTask request)
            |> Task.andThen (\_ -> Http.toTask <| listRequest e.rubric_id)
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
        renderItem i =
            div [ class "clearfix" ]
                [ renderIf adminMode <| deleteButton <| DeleteFromList listField e i 
                , text i
                ]
    in
        div []
            [ h3 [] [text <| getFieldLabel listField]
            , div [] (List.map renderItem items)
            , addForm (getField rec listField) (AddToList listField e) (ChangeListField listField) title
                |> renderIf adminMode
            ]

makeMenuItem : Entity -> MenuItem Msg
makeMenuItem e =
    { name = e.name
    , href = "#/" ++ toString e.rubric_id ++ "/" ++ toString e.id
    , onClick = ChangeCurrent e
    , onDelete = Delete e
    }

render : Model -> Bool -> Rubric.Entity -> Html Msg
render model adminMode rubric =
    let
        renderLF = renderListField adminMode model.listFields
        listFields s = List.map (renderLF s) [Preinvestigation, Investigation, Trial]
        menu = renderMenu adminMode <| List.map makeMenuItem model.list
        form = renderIf adminMode <| addForm model.name (Add rubric) ChangeName "Условие"
        content = case model.current of
            Just s ->
                h2 [] [text s.name]
                :: listFields s
            Nothing -> []
    in 
        div []
            [ menu
            , form
            , div [class "sample-content"] content
            ]

