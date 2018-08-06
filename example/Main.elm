module Main exposing (..)

import Field as F
import Field.Int as FInt
import Field.String as FStr
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Task


type alias Model =
    { name : FStr.Field
    , email : FStr.Field
    , age : FInt.Field
    , extraInfo : FStr.Field
    , submittedSuccessfully : Bool
    }


init : ( Model, Cmd Msg )
init =
    { name = F.init ""
    , email = F.init ""
    , age = F.init 0
    , extraInfo = F.init ""
    , submittedSuccessfully = False
    }
        ! []


type Msg
    = SetNameField String
    | SetEmailField String
    | SetAgeField String
    | ToggleExtraInfoDisabled
    | SetExtraInfoField String
    | Submit
    | SubmitResponse Bool


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetNameField value ->
            { model
                | name =
                    value
                        |> F.resetValue model.name
                        |> validateName
            }
                ! []

        SetEmailField value ->
            { model
                | email =
                    value
                        |> F.resetValue model.email
                        |> validateEmail
            }
                ! []

        SetAgeField value ->
            { model
                | age =
                    value
                        |> (String.toInt >> Result.withDefault 0)
                        |> F.resetValue model.age
                        |> validateAge
            }
                ! []

        ToggleExtraInfoDisabled ->
            let
                metadata =
                    F.extractMetadata model.extraInfo
            in
            { model
                | extraInfo =
                    F.resetMetadata
                        model.extraInfo
                        { metadata | disabled = not metadata.disabled }
            }
                ! []

        SetExtraInfoField value ->
            { model
                | extraInfo =
                    value
                        |> F.resetValue model.extraInfo
                        |> validateExtraInfo
            }
                ! []

        Submit ->
            let
                name =
                    validateName model.name

                email =
                    validateEmail model.email

                age =
                    validateAge model.age

                extraInfo =
                    validateExtraInfo model.extraInfo

                cmds =
                    Result.map4
                        (\nameValue emailValue ageValue extraInfoValue ->
                            [ -- Mock a server request
                              Task.perform SubmitResponse (Task.succeed True)
                            ]
                        )
                        (F.toResult name)
                        (F.toResult email)
                        (F.toResult age)
                        (F.toResult extraInfo)
                        |> Result.withDefault
                            [ -- Mock a server request
                              Task.perform SubmitResponse (Task.succeed False)
                            ]
            in
            { model
                | name = name
                , email = email
                , age = age
                , extraInfo = extraInfo
            }
                ! cmds

        SubmitResponse response ->
            { model | submittedSuccessfully = response } ! []


validateName : FStr.ValidationFunc
validateName =
    FStr.notEmpty


validateEmail : FStr.ValidationFunc
validateEmail =
    FStr.notEmpty >> FStr.email


validateAge : FInt.ValidationFunc
validateAge =
    FInt.greaterThanOrEqual 18 >> FInt.lessThan 100


validateExtraInfo : FStr.ValidationFunc
validateExtraInfo =
    FStr.optional (FStr.numeric >> not15)


not15 : FStr.ValidationFunc
not15 =
    F.test
        (\value ->
            if value /= "15" then
                True
            else
                False
        )
        "Extra Info can't be \"15\""


view : Model -> Html Msg
view model =
    Html.div []
        [ Html.h1 [] [ Html.text "Sign Up!" ]
        , F.view (stringFieldConfig "Name" SetNameField) model.name
        , F.view (stringFieldConfig "Email" SetEmailField) model.email
        , F.view (intFieldConfig "Age" SetAgeField) model.age
        , Html.button [ Html.Events.onClick ToggleExtraInfoDisabled ]
            [ Html.span [] [ Html.text "Toggle Extra Info Disabled" ] ]
        , F.view (stringFieldConfig "Extra Info" SetExtraInfoField)
            model.extraInfo
        , Html.button [ Html.Events.onClick Submit ]
            [ Html.span [] [ Html.text "Submit" ] ]
        , Html.div [] <|
            if model.submittedSuccessfully then
                [ Html.span [] [ Html.text "Submited Successfully!" ] ]
            else
                []
        ]


stringFieldConfig : String -> (String -> msg) -> FStr.ViewConfig msg
stringFieldConfig title toMsg =
    { valid =
        \meta value ->
            Html.div []
                [ Html.input
                    [ Html.Events.onInput toMsg
                    , Html.Attributes.value value
                    , Html.Attributes.disabled meta.disabled
                    ]
                    []
                ]
    , invalid =
        \meta value errorMessage ->
            Html.div []
                [ Html.input
                    [ Html.Events.onInput toMsg
                    , Html.Attributes.value value
                    , Html.Attributes.disabled meta.disabled
                    ]
                    []
                , Html.span []
                    [ Html.text errorMessage ]
                ]
    }


intFieldConfig : String -> (String -> msg) -> FInt.ViewConfig msg
intFieldConfig title toMsg =
    { valid =
        \meta value ->
            Html.div []
                [ Html.input
                    [ Html.Events.onInput toMsg
                    , Html.Attributes.value (toString value)
                    , Html.Attributes.type_ "number"
                    , Html.Attributes.disabled meta.disabled
                    ]
                    []
                ]
    , invalid =
        \meta value errorMessage ->
            Html.div []
                [ Html.input
                    [ Html.Events.onInput toMsg
                    , Html.Attributes.value (toString value)
                    , Html.Attributes.type_ "number"
                    , Html.Attributes.disabled meta.disabled
                    ]
                    []
                , Html.span []
                    [ Html.text errorMessage ]
                ]
    }


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
