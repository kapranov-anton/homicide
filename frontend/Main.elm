import Html exposing (Html, button, div, text)
import Html.Events exposing (..)
import Json.Decode exposing (..)
import Http


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Rubric =
    { id: Int
    , name: String
    }

type alias Sample =
    { id: Int
    , name: String
    , rubric_id: Int
    , preinvestigation: List String
    , investigation: List String
    , trial: List String
    }


-- MODEL

type alias Model =
    { rubricList: List Rubric
    , sampleList: List Sample
    , currentRubric: Maybe Rubric
    , currentSample: Maybe Sample
    }

init : (Model, Cmd Msg)
init = (
    { rubricList = []
    , sampleList = []
    , currentRubric = Nothing
    , currentSample = Nothing
    }, getRubricList)


-- UPDATE

type Msg
    = Reset
    | LoadRubricList (Result Http.Error (List Rubric))
    | LoadSampleList (Result Http.Error (List Sample))
    | ChangeRubric Rubric
    | ChangeSample Sample


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Reset -> ({ model | currentRubric = Nothing }, Cmd.none)
    ChangeRubric r -> ({ model | currentRubric = Just r }, Cmd.none)
    ChangeSample s -> ({ model | currentSample = Just s }, Cmd.none)
    LoadRubricList (Ok rs) -> ({ model | rubricList = rs }, Cmd.none)
    LoadRubricList (Err _) -> (model, Cmd.none)
    LoadSampleList (Ok ss) -> ({ model | sampleList = ss }, Cmd.none)
    LoadSampleList (Err _) -> (model, Cmd.none)


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none


-- VIEW

renderRubricList rs = div [] (List.map (\x -> text x.name) rs)

view : Model -> Html Msg
view
    { rubricList
    , sampleList
    , currentRubric
    , currentSample
    } =
    case currentRubric of
        Just r -> text r.name
        Nothing -> renderRubricList rubricList


-- ACTIONS

rubricDecoder = map2 Rubric (field "id" int) (field "name" string)
sampleDecoder = map6 Sample
    (field "id" int)
    (field "name" string)
    (field "rubric_id" int)
    (field "preinvestigation" (list string))
    (field "investigation" (list string))
    (field "trial" (list string))

backendUrl = "http://localhost:3000" -- /rubric 

getRubricList : Cmd Msg
getRubricList =
    let
        url = backendUrl ++ "/rubric"
        request = Http.get url (list rubricDecoder)
    in
        Http.send LoadRubricList request
