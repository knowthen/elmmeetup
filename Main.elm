module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


-- Model


type alias Model =
    { name : String, ip : String, id : Int }


initModel : Model
initModel =
    { name = "", ip = "127.0.0.1", id = 2 }


types =
    [ ( 1, "Light" ), ( 2, "Ceiling" ) ]


options model =
    List.map (\( id, name ) -> option [ value (toString id), selected (id == model.id) ] [ text name ]) types



-- Update


type Msg
    = NameInput String
    | IpInput String
    | TypeInput String



-- 2. Gets our messagge NameInput "Jam"


update : Msg -> Model -> Model
update msg model =
    case msg of
        NameInput name ->
            { model | name = name }

        IpInput ip ->
            { model | ip = ip }

        TypeInput id ->
            let
                idInteger =
                    case String.toInt id of
                        Result.Ok integer ->
                            integer

                        Result.Err err ->
                            0
            in
                { model | id = idInteger }



-- myOnInput name =
--     NameInput name
--
-- View
-- 1. NameInput Is created & and sent to Update


view model =
    -- <input type="text" value="asdfa" >asdfasdf</input>
    div []
        [ input [ type_ "text", value model.name, onInput NameInput ] []
        , input [ type_ "text", value model.ip, onInput IpInput ] []
        , select [ onInput TypeInput ] (options model)
        ]


main =
    Html.beginnerProgram { model = initModel, update = update, view = view }
