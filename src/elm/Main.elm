module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List exposing (head)


-- Model


type alias Device =
    { name : String
    , ip : String
    , id : String
    , type_ : Int
    , isOn : Bool
    }


newDevice : Device
newDevice =
    { name = ""
    , ip = ""
    , id = ""
    , type_ = 0
    , isOn = False
    }


type alias Model =
    { device : Device
    , devices : List Device
    }


initModel : Model
initModel =
    { device = newDevice
    , devices = []
    }



-- Update


type Msg
    = NameInput String
    | IpInput String
    | TypeInput String
    | Save


update : Msg -> Model -> Model
update msg model =
    case msg of
        NameInput name ->
            nameInput name model

        IpInput ip ->
            ipInput ip model

        TypeInput type_ ->
            typeInput type_ model

        Save ->
            model


nameInput : String -> Model -> Model
nameInput name model =
    let
        device =
            model.device

        updatedDevice =
            { device | name = name }
    in
        { model | device = updatedDevice }


ipInput : String -> Model -> Model
ipInput ip model =
    let
        device =
            model.device

        updatedDevice =
            { device | ip = ip }
    in
        { model | device = updatedDevice }


typeInput : String -> Model -> Model
typeInput type_ model =
    let
        intType_ =
            String.toInt type_
                |> Result.withDefault 0

        device =
            model.device

        updatedDevice =
            { device | type_ = intType_ }
    in
        { model | device = updatedDevice }



-- View


view : Model -> Html Msg
view model =
    div [ class "pa1 pa2-m ph5-m pa3-l ph6-l" ]
        [ viewAddSection model
        , viewDeviceSection "Devices" model.devices
        ]


viewAddSection : Model -> Html Msg
viewAddSection model =
    main_ [ class "fl w-100 w-50-l pr3-l black-80" ]
        [ Html.form [ class "measure ", onSubmit Save ]
            [ fieldset [ class "ba b--transparent ph0 mh0" ]
                [ legend [ class "f3 fw6 ph0 mh0 pv2 b--black bb dib w-100" ]
                    [ text "Add Item" ]
                , viewFormFieldInputGroup "Name" NameInput model.device.name
                , viewFormFieldInputGroup "IP" IpInput model.device.ip
                , viewFormFieldSelectGroup "Type" TypeInput <|
                    deviceOptions model.device.type_
                ]
            , div []
                [ input
                    [ class buttonClasses
                    , type_ "submit"
                    , value "Save"
                    ]
                    []
                ]
            ]
        ]


viewFormFieldInputGroup : String -> (String -> Msg) -> String -> Html Msg
viewFormFieldInputGroup lbl msg val =
    div [ class "mt3" ]
        [ label [ class labelClasses ]
            [ text lbl ]
        , input
            [ type_ "text"
            , class inputClasses
            , onInput msg
            , value val
            ]
            []
        ]


viewFormFieldSelectGroup : String -> (String -> Msg) -> List (Html Msg) -> Html Msg
viewFormFieldSelectGroup lbl msg ops =
    div [ class "mv3" ]
        [ label [ class labelClasses ]
            [ text lbl ]
        , select
            [ class <| inputClasses
            , onInput msg
            ]
            ops
        ]


viewDeviceSection : String -> List Device -> Html Msg
viewDeviceSection heading devices =
    div [ class "fl w-100 w-50-l pr3-l" ]
        [ h2 [ class "b--black bb pv2 ma0" ]
            [ text heading ]
        , viewDeviceList devices
        ]


viewDeviceList : List Device -> Html Msg
viewDeviceList devices =
    devices
        |> List.sortBy .name
        |> List.map (\d -> viewDevice d)
        |> ul [ class "list pl0 mt0" ]


viewDevice : Device -> Html Msg
viewDevice device =
    li [ class "lh-copy pv3 ph0-l bb b--black-10" ]
        [ div [ class "flex w-100  lh-copy ph0-l " ]
            [ div [ class "fl w-75 br-100 f3" ]
                [ text device.name ]
            , div [ class "fl w-25 pl3 " ]
                [ a
                    [ class <| "fr f6 link pointer dim ba bw1 ph3 pv1 dib " ++ toggleButtonColor device.isOn
                    ]
                    [ text <| onDescription device.isOn ]
                ]
            ]
        , div [ class "flex w-100 ph0-l mt1 " ]
            [ div [ class "fl w-75 br-100 f6" ]
                [ text <| "(" ++ device.ip ++ ") " ++ (deviceTypeToName device.type_)
                ]
            , div [ class "fl w-25 pl3 " ]
                [ a
                    [ class "fr f6 link pointer dim ph3 dib black"
                    ]
                    [ text "Edit" ]
                ]
            ]
        ]


onDescription : Bool -> String
onDescription isOn =
    if isOn then
        "Off"
    else
        "On"


toggleButtonColor : Bool -> String
toggleButtonColor isOn =
    if isOn then
        "red"
    else
        "green"


deviceTypes : List ( Int, String )
deviceTypes =
    [ ( 0, "" )
    , ( 1, "Light" )
    , ( 2, "Ceiling" )
    ]


deviceTypeToName : Int -> String
deviceTypeToName deviceType =
    deviceTypes
        |> List.filter (\( id, _ ) -> id == deviceType)
        |> List.map (\( _, name ) -> name)
        |> head
        |> Maybe.withDefault ""


deviceOptions : Int -> List (Html Msg)
deviceOptions i =
    List.map
        (\( i_, name ) ->
            option
                [ value (toString i_)
                , selected (i == i_)
                ]
                [ text name ]
        )
        deviceTypes


labelClasses : String
labelClasses =
    "db fw6 lh-copy f5"


inputClasses : String
inputClasses =
    "pa2 input-reset ba b--black w-100 outline-0 f4 br0"


buttonClasses : String
buttonClasses =
    "b mr2 ph3 pv2 input-reset ba b--black bg-transparent grow pointer f4 dib"


main : Program Never Model Msg
main =
    Html.beginnerProgram
        { model = initModel
        , update = update
        , view = view
        }
