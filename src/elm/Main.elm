module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List exposing (head)


-- Model


type Page
    = Home
    | AddEdit (Maybe Int)


type alias Device =
    { name : String
    , ip : String
    , id : Int
    , type_ : Int
    , isOn : Bool
    }


newDevice : Device
newDevice =
    { name = ""
    , ip = ""
    , id = 0
    , type_ = 0
    , isOn = False
    }


type alias Model =
    { device : Device
    , editId : Maybe Int
    , devices : List Device
    , nextId : Int
    , page : Page
    }


initModel : Model
initModel =
    { device = newDevice
    , editId = Nothing
    , devices = []
    , nextId = 0
    , page = Home
    }


deviceTypes : List ( Int, String )
deviceTypes =
    [ ( 0, "" ), ( 1, "Light" ), ( 2, "Ceiling" ) ]


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



-- Update


type Msg
    = NameInput String
    | IpInput String
    | TypeInput String
    | PageChange Page
    | Toggle Int
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

        PageChange Home ->
            { model | page = Home, device = newDevice, editId = Nothing }

        PageChange (AddEdit Nothing) ->
            { model | page = AddEdit Nothing }

        PageChange (AddEdit (Just id)) ->
            pageChange id model

        Save ->
            save model

        Toggle id ->
            toggle id model


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


pageChange : Int -> Model -> Model
pageChange id model =
    let
        device =
            model.devices
                |> List.filter (\d -> d.id == id)
                |> head
                |> Maybe.withDefault newDevice
    in
        { model | page = AddEdit <| Just id, editId = Just id, device = device }


toggle : Int -> Model -> Model
toggle id model =
    let
        devices =
            model.devices
                |> List.map
                    (\d ->
                        if d.id == id then
                            { d | isOn = not d.isOn }
                        else
                            d
                    )
    in
        { model | devices = devices }


save : Model -> Model
save model =
    case model.editId of
        Just id ->
            editDevice id model

        Nothing ->
            addDevice model


editDevice : Int -> Model -> Model
editDevice id model =
    let
        updatedDevices =
            List.map
                (\d ->
                    if d.id == id then
                        model.device
                    else
                        d
                )
                model.devices
    in
        { model
            | devices = updatedDevices
            , editId = Nothing
            , page = Home
        }


addDevice : Model -> Model
addDevice model =
    let
        device =
            model.device

        updatedDevice =
            { device | id = model.nextId }
    in
        { model
            | devices = updatedDevice :: model.devices
            , device = newDevice
            , nextId = model.nextId + 1
            , page = Home
        }



-- View


labelClasses : String
labelClasses =
    "db fw6 lh-copy f5"


inputClasses : String
inputClasses =
    "pa2 input-reset ba b--black w-100 outline-0 f4 br0"


buttonClasses : String
buttonClasses =
    "b mr2 ph3 pv2 input-reset ba b--black bg-transparent grow pointer f4 dib"


navLinkClasses : String
navLinkClasses =
    "link pointer dim dark-gray f6 f5-l dib mr3 mr4-l pa2"


navClasses : String
navClasses =
    "db dt-l w-100 border-box pa1 ph2 pa3-l ph6-l"


navLogoLinkClasses : String
navLogoLinkClasses =
    "db pointer dtc-l v-mid mid-gray link dim w-100 w-25-l tc tl-l mb2 mb0-l"


navLinks : List ( String, Page )
navLinks =
    [ ( "Devices", Home )
    , ( "Add Device", AddEdit Nothing )
    ]


viewNavLink : Page -> List ( String, Page ) -> List (Html Msg)
viewNavLink currentPage links =
    List.map
        (\( name, page ) ->
            let
                linkClasses =
                    if page == currentPage then
                        navLinkClasses ++ " bb b--black bw1"
                    else
                        navLinkClasses
            in
                a
                    [ class linkClasses
                    , onClick <| PageChange page
                    ]
                    [ text name ]
        )
        links


viewNav : Page -> Html Msg
viewNav page =
    let
        links =
            viewNavLink page navLinks
    in
        nav [ class navClasses ]
            [ a
                [ class navLogoLinkClasses
                , onClick <| PageChange Home
                ]
                [ img [ class "dib w2 h2 br-100", src "static/logo.png" ]
                    []
                ]
            , div [ class "db dtc-l v-mid w-100 w-75-l tc tr-l" ]
                links
            ]


view : Model -> Html Msg
view model =
    let
        page =
            case model.page of
                Home ->
                    viewHome model

                AddEdit _ ->
                    viewEditAdd model
    in
        div []
            [ viewNav model.page
            , page
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


viewDevice : Device -> Html Msg
viewDevice device =
    li [ class "lh-copy pv3 ph0-l bb b--black-10" ]
        [ div [ class "flex w-100  lh-copy ph0-l " ]
            [ div [ class "fl w-75 br-100 f3" ]
                [ text device.name ]
            , div [ class "fl w-25 pl3 " ]
                [ a
                    [ class <| "fr f6 link pointer dim ba bw1 ph3 pv1 dib " ++ toggleButtonColor device.isOn
                    , onClick <| Toggle device.id
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
                    , onClick <| PageChange <| AddEdit <| Just device.id
                    ]
                    [ text "Edit" ]
                ]
            ]
        ]


viewHomeDeviceList : List Device -> Html Msg
viewHomeDeviceList devices =
    devices
        |> List.sortBy .name
        |> List.map (\d -> viewDevice d)
        |> ul [ class "list pl0 mt0" ]


viewHomeDeviceSection : String -> List Device -> Html Msg
viewHomeDeviceSection heading devices =
    div [ class "fl w-100 w-50-l pr3-l" ]
        [ h2 [ class "b--black bb pb2" ]
            [ text heading ]
        , viewHomeDeviceList devices
        ]


viewHome : Model -> Html Msg
viewHome model =
    div [ class "pa1 pa2-m ph5-m pa3-l ph6-l" ]
        [ viewHomeDeviceSection "Off" <|
            List.filter (\d -> d.isOn == False) model.devices
        , viewHomeDeviceSection "On" <|
            List.filter (\d -> d.isOn == True) model.devices
        ]


addEditHeading : Page -> String
addEditHeading page =
    case page of
        AddEdit Nothing ->
            "Add"

        _ ->
            "Edit"


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


viewEditAdd : Model -> Html Msg
viewEditAdd model =
    main_ [ class "pa1 pa2-m pa4-l black-80" ]
        [ Html.form [ class "measure center", onSubmit Save ]
            [ fieldset [ class "ba b--transparent ph0 mh0" ]
                [ legend [ class "f3 fw6 ph0 mh0" ]
                    [ text <| addEditHeading model.page ++ " Item" ]
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
                , input
                    [ class buttonClasses
                    , type_ "button"
                    , value "Cancel"
                    , onClick <| PageChange Home
                    ]
                    []
                ]
            ]
        ]


main : Program Never Model Msg
main =
    Html.beginnerProgram
        { model = initModel
        , update = update
        , view = view
        }
