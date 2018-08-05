module Main exposing (..)

import Html exposing (Html)
import Html.Attributes as Attrs
import Html.Events as Events
import Field as F
import Field.String as FStr
import Field.Int as FInt


type alias Model =
    { firstNameField : FStr.StringField
    , lastNameField : FStr.StringField
    , emailField : FStr.StringField
    , ageField : FInt.IntField
    }


init : Model
init =
    { firstNameField = F.new "First Name" ""
    , lastNameField = F.new "Last Name" ""
    , emailField = F.new "Email" ""
    , ageField = F.new "Age" 0
    }


type Msg
    = SetFirstNameField String
    | SetLastNameField String
    | SetEmailField String
    | SetAgeField String


update : Msg -> Model -> Model
update msg model =
    case msg of
        SetFirstNameField newValue ->
            { model
                | firstNameField =
                    F.set model.firstNameField newValue
                        |> firstNameValidation
            }

        SetLastNameField newValue ->
            { model
                | lastNameField =
                    F.set model.lastNameField newValue
                        |> lastNameValidation
            }

        SetEmailField newValue ->
            { model
                | emailField =
                    F.set model.emailField newValue
                        |> emailValidation
            }

        SetAgeField newValue ->
            case String.toInt newValue of
                Ok num ->
                    { model
                        | ageField =
                            F.set model.ageField num
                                |> ageValidation
                    }

                Err _ ->
                    { model
                        | ageField =
                            F.Invalid (F.extractMeta model.ageField) "Not a number"
                    }


firstNameValidation : FStr.StringValidationFunc
firstNameValidation =
    FStr.required


lastNameValidation : FStr.StringValidationFunc
lastNameValidation =
    FStr.required


emailValidation : FStr.StringValidationFunc
emailValidation =
    FStr.required >> FStr.email


ageValidation : FInt.IntValidationFunc
ageValidation =
    FInt.greaterThan 18


view : Model -> Html Msg
view model =
    Html.div
        [ Attrs.style
            [ ( "display", "flex" )
            , ( "flex", "1" )
            , ( "flex-direction", "column" )
            , ( "justify-content", "space-around" )
            , ( "alignItems", "center" )
            , ( "height", "40vh" )
            ]
        ]
        [ Html.h1 []
            [ Html.text "Form Example" ]
        , viewInput SetFirstNameField identity model.firstNameField
        , viewInput SetLastNameField identity model.lastNameField
        , viewInput SetEmailField identity model.emailField
        , viewInput SetAgeField toString model.ageField
        ]


inputStyle =
    [ ( "width", "40vw" ) ]


viewInput : (String -> msg) -> (message -> String) -> F.Field String message -> Html msg
viewInput onInputMsg valueToString field =
    case field of
        F.Valid meta ->
            Html.div []
                [ Html.div []
                    [ Html.span [] [ Html.text meta.name ] ]
                , Html.input
                    [ Events.onInput onInputMsg
                    , Attrs.value (valueToString meta.value)
                    , Attrs.style inputStyle
                    ]
                    []
                ]

        F.Invalid meta errorMessage ->
            Html.div []
                [ Html.div []
                    [ Html.span [] [ Html.text meta.name ]
                    , Html.span
                        [ Attrs.style
                            [ ( "marginLeft", "1vw" )
                            , ( "color", "#ff6978" )
                            ]
                        ]
                        [ Html.text errorMessage ]
                    ]
                , Html.input
                    [ Events.onInput onInputMsg
                    , Attrs.value (valueToString meta.value)
                    , Attrs.style inputStyle
                    ]
                    []
                ]

        F.Disabled meta ->
            Html.div []
                [ Html.div []
                    [ Html.span [] [ Html.text meta.name ] ]
                , Html.input
                    [ Attrs.value (valueToString meta.value)
                    , Attrs.disabled True
                    , Attrs.style inputStyle
                    ]
                    []
                ]


main =
    Html.beginnerProgram
        { model = init
        , view = view
        , update = update
        }
