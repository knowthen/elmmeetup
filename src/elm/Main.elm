module Main exposing (..)

import Html exposing (..)


-- MODEL


type alias Model =
    Int


model : number
model =
    0



-- UPDATE


type Msg
    = Increment


update : Msg -> Model -> Model
update msg model =
    case msg of
        Increment ->
            model + 1



-- VIEW


view : Model -> Html Msg
view model =
    div [] [ text "hello" ]


main : Program Never Int Msg
main =
    Html.beginnerProgram { model = model, view = view, update = update }
