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


inputClass : String
inputClass =
    "pa2 input-reset ba b--black w-100 outline-0 f4"


view model =
    -- div []
    --     [ input [ type_ "text", value model.name, onInput NameInput ] []
    --     , input [ type_ "text", value model.ip, onInput IpInput ] []
    --     , select [ onInput TypeInput ] (options model)
    --     ]
    main_ [ class "pa4 black-80" ]
        [ Html.form [ class "measure center" ]
            [ fieldset [ class "ba b--transparent ph0 mh0" ]
                [ legend [ class "f3 fw6 ph0 mh0" ]
                    [ text "Add / Edit Item" ]
                , div [ class "mt3" ]
                    [ label [ class "db fw6 lh-copy f5" ]
                        [ text "Name" ]
                    , input
                        [ type_ "text"
                        , class inputClass
                        , onInput NameInput
                        , value model.name
                        ]
                        []
                    ]
                , div [ class "mv3" ]
                    [ label [ class "db fw6 lh-copy f5" ]
                        [ text "IP" ]
                    , input
                        [ type_ "text"
                        , class inputClass
                        , onInput IpInput
                        , value model.ip
                        ]
                        []
                    ]
                , div [ class "mv3" ]
                    [ label [ class "db fw6 lh-copy f5" ]
                        [ text "Type" ]
                    , select
                        [ class <| inputClass ++ " br0"
                        , onInput TypeInput
                        ]
                        (options model)
                    ]
                ]
            , div [ class "" ]
                [ input
                    [ class "b ph3 pv2 input-reset ba b--black bg-transparent grow pointer f4 dib"
                    , type_ "submit"
                    , value "Save"
                    ]
                    []
                ]
            ]
        ]


main =
    Html.beginnerProgram { model = initModel, update = update, view = view }
