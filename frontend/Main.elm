import Html exposing (Html, button, div, text, a, ul, li)
import Html.Events exposing (onClick)
import Html.Attributes exposing (href)
import Json.Decode exposing (..)
import Http
import Debug exposing (log)


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
    , adminMode: Bool
    }

init : (Model, Cmd Msg)
init = (
    { rubricList = []
    , sampleList = []
    , currentRubric = Nothing
    , currentSample = Nothing
    , adminMode = False
    }, getRubricList)


-- UPDATE

type Msg
    = Reset
    | SetAdminMode
    | LoadRubricList (Result Http.Error (List Rubric))
    | LoadSampleList (Result Http.Error (List Sample))
    | ChangeRubric Rubric
    | ChangeSample Sample


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Reset -> ({ model | currentRubric = Nothing, currentSample = Nothing, sampleList = [] }, Cmd.none)
    SetAdminMode -> ({model | adminMode = True}, Cmd.none)
    ChangeRubric r -> ({ model | currentRubric = Just r }, getSampleList r)
    ChangeSample s -> ({ model | currentSample = Just s }, Cmd.none)
    LoadRubricList (Ok rs) -> ({ model | rubricList = rs }, Cmd.none)
    LoadRubricList (Err _) -> (model, Cmd.none)
    LoadSampleList (Ok ss) -> ({ model | sampleList = ss }, Cmd.none)
    LoadSampleList (Err _) -> (model, Cmd.none)


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none


-- VIEW

renderRubric r = li []
    [ a [href ("#/" ++ toString r.id), onClick (ChangeRubric r)]
        [text r.name]
    ]
renderRubricList rs = ul [] (List.map renderRubric rs)

renderSample s = li []
    [ a [ href ("#/" ++ toString s.rubric_id ++ "/" ++ toString s.id)
        , onClick (ChangeSample s)]
        [text s.name]
    ]
renderSampleList ls = ul [] (List.map renderSample ls)

renderCurrentRubric rubricList currentRubric =
    case currentRubric of
        Just r ->
            div []
                [ a [href "#/", onClick Reset]
                    [text "Назад"]
                , text r.name
                ]
        Nothing -> renderRubricList rubricList

renderCurrentSample sampleList currentSample =
    case currentSample of
        Just s ->
            div []
                [ text s.name
                ]
        Nothing -> div [] []

renderLogin adminMode =
    if adminMode then
        div [] []
    else
        button [onClick SetAdminMode]
            [text "Войти в режим администратора"]

view : Model -> Html Msg
view
    { rubricList
    , sampleList
    , currentRubric
    , currentSample
    , adminMode
    } = 
    div []
        [ renderLogin adminMode
        , renderCurrentRubric rubricList currentRubric
        , renderSampleList sampleList
        , renderCurrentSample sampleList currentSample
        ]

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

getSampleList : Rubric -> Cmd Msg
getSampleList r =
    let
        url = backendUrl ++ "/sample?rubric_id=eq." ++ toString r.id
        request = Http.get url (list sampleDecoder)
    in
        Http.send LoadSampleList request
