module Main exposing (Model, Msg(..), init, intFieldConfig, main, notAbc, stringFieldConfig, update, validateAge, validateEmail, validateExtraInfo, validateName, view)

import Browser exposing (Document)
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


type alias Flags =
    {}


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( { name = F.init ""
      , email = F.init ""
      , age = F.init 0
      , extraInfo = F.init ""
      , submittedSuccessfully = False
      }
    , Cmd.none
    )


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
            ( { model
                | name =
                    value
                        |> F.resetValue model.name
                        |> validateName
              }
            , Cmd.none
            )

        SetEmailField value ->
            ( { model
                | email =
                    value
                        |> F.resetValue model.email
                        |> validateEmail
              }
            , Cmd.none
            )

        SetAgeField value ->
            ( { model
                | age =
                    value
                        |> (String.toInt >> Maybe.withDefault 0)
                        |> F.resetValue model.age
                        |> validateAge
              }
            , Cmd.none
            )

        ToggleExtraInfoDisabled ->
            let
                metadata =
                    F.extractMetadata model.extraInfo
            in
            ( { model
                | extraInfo =
                    F.resetMetadata
                        model.extraInfo
                        { metadata | disabled = not metadata.disabled }
              }
            , Cmd.none
            )

        SetExtraInfoField value ->
            ( { model
                | extraInfo =
                    value
                        |> F.resetValue model.extraInfo
                        |> validateExtraInfo
              }
            , Cmd.none
            )

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
                    Result.withDefault [] <|
                        Result.map4
                            (\nameValue emailValue ageValue extraInfoValue ->
                                [ -- Mock a server request
                                  Task.perform SubmitResponse
                                    (Task.succeed True)
                                ]
                            )
                            (F.toResult name)
                            (F.toResult email)
                            (F.toResult age)
                            (F.toResult extraInfo)
            in
            ( { model
                | name = name
                , email = email
                , age = age
                , extraInfo = extraInfo
              }
            , Cmd.batch cmds
            )

        SubmitResponse response ->
            ( { model | submittedSuccessfully = response }, Cmd.none )


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
    FStr.optional (FStr.nonnumeric >> notAbc)


notAbc : FStr.ValidationFunc
notAbc =
    F.createValidator
        (\value ->
            if value /= "abc" then
                True

            else
                False
        )
        "Can't be \"abc\""


view : Model -> List (Html Msg)
view model =
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
                    , Html.Attributes.value (String.fromInt value)
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
                    , Html.Attributes.value (String.fromInt value)
                    , Html.Attributes.type_ "number"
                    , Html.Attributes.disabled meta.disabled
                    ]
                    []
                , Html.span []
                    [ Html.text errorMessage ]
                ]
    }


main =
    Browser.document
        { init = init
        , view =
            \model ->
                { title = "elm-field example", body = view model }
        , update = update
        , subscriptions = \_ -> Sub.none
        }
