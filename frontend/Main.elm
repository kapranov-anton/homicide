import Html exposing (Html, button, div, text, a, ul, li, input)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (href, type_, placeholder)
import Json.Decode exposing (..)
import Json.Encode as JE
import Http
import Task
import Helpers exposing (noDecodePost, renderJust, renderIf)

import Rubric
import Sample
import Api


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

-- MODEL

type alias Model =
    { rubric: Rubric.Model
    , sample: Sample.Model
    , adminMode: Bool
    }


init : (Model, Cmd Msg)
init = (
    { rubric = Rubric.init
    , sample = Sample.init
    , adminMode = False
    }, Cmd.map RubricMsg Rubric.getList) -- getRubricList


-- UPDATE

type Msg
    = SetAdminMode
    | RubricMsg Rubric.Msg
    | SampleMsg Sample.Msg


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    SetAdminMode -> ({model | adminMode = True}, Cmd.none)
    RubricMsg subMsg ->
        let
            (subModel, subCmd) = Rubric.update subMsg model.rubric
            newModel = { model | rubric = subModel }
            newCmd = Cmd.map RubricMsg subCmd
        in case subMsg of
            Rubric.Reset ->
                { newModel | sample = Sample.init } ! [newCmd]
            Rubric.ChangeCurrent r ->
                newModel ! [newCmd, Cmd.map SampleMsg <| Sample.getList r]
            _ ->
                newModel ! [newCmd]
    SampleMsg subMsg ->
        let
            (newModel, cmd) = Sample.update subMsg model.sample
        in
            ({ model | sample = newModel }, Cmd.map SampleMsg cmd)


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none


-- VIEW

renderLogin adminMode =
    renderIf (not adminMode) <|
        button [onClick SetAdminMode]
            [text "Войти в режим администратора"]

view : Model -> Html Msg
view { rubric, sample, adminMode } =
    div []
        [ renderLogin adminMode
        , Html.map RubricMsg <| Rubric.render rubric adminMode
        , Html.map SampleMsg <| renderJust rubric.current <| Sample.render sample adminMode
        ]

