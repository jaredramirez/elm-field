module Field
    exposing
        ( Field
        , Metadata
        , ValidationFunc
        , ViewConfig
        , extractMetadata
        , extractValue
        , init
        , isInvalid
        , isValid
        , noValidation
        , resetMetadata
        , resetValue
        , test
        , toMaybe
        , toResult
        , view
        , withDefault
        )

{-| This library provides a datatype to model input field data.

To use this data type, let's say that you need a sign up form that has a requried name field,
a required email field, and an age field that must be between 18 & 100 that you need to send
to your server after it's validated.

First, you can import the package and create the fields in your model

    ... other imports
    import Elm.Field as F
    import Elm.Field.String as FStr
    import Elm.Field.Int as FInt

    type alias Model =
        { name : FStr.Field
        , email : FStr.Field
        , age : FInt.Field
        }

    init : Model
    init =
        { name = F.init ""
        , email = F.init ""
        , age = F.init 0
        }

Then, you add a few messages to update the fields, and one to submit your form

    type Msg
        = SetNameField String
        | SetEmailField String
        | SetAgeField Int
        | Submit

Next, you add logic to set & validate the fields to your update function

    update : Msg -> Model -> (Model, Cmd Msg)
    update msg model =
        case msg of
            SetNameField value ->
                { model
                    | name =
                        value
                            |> F.resetValue model.name
                            |> validateName
                } ! []

            SetEmailField value ->
                { model
                    | email =
                        value
                            |> F.resetValue model.email
                            |> validateEmail
                } ! []

            SetAgeField value ->
                { model
                    | age =
                        value
                            |> F.resetValue model.age
                            |> validateAge
                } ! []

            Submit ->
                let
                    name = validateName model.name

                    email = validateEmail model.email

                    age = validateEmail model.age

                    cmds=
                        case (F.toResult name, F.toResult email, F.toResult age) of
                            (Ok nameValue, Ok, emailValue, Ok ageValue) ->
                                [ ...some command... ]

                            _ ->
                                []

                in
                    { model
                        | name = name
                        , email = email
                        , age = age
                    } ! cmds

    validateName : F.ValidationFunc
    validateName =
        FStr.notEmpty

    validateEmail : F.ValidationFunc
    validateEmail =
        FStr.notEmpty >> FStr.email

    validateAge : F.ValidationFunc
    validateAge =
        FInt.greaterThan 18 >> FInt.atMost 100

Finally, wire it into the view!

    view : Model -> Html Msg
    view model =
        Html.div []
            [ Html.h1 [] [ Html.text "Sign Up!" ]
            , F.view (fieldConfig "Name" SetNameField) model.name
            , F.view (fieldConfig "Email" SetEmailField) model.email
            , F.view (fieldConfig "Age" SetAgeField) model.age
            ]

        viewConfig : String -> msg -> F.ViewConfig
        viewConfig title msg =
            { valid  =
                \meta value ->
                    Html.div []
                        [ Html.input
                            [ Html.Events.onClick msg
                            , Html.Attributes.value value
                            , Html.Attributes.disabled meta.disabled
                            ]
                            []
                        ]

            , invalid =
                \meta value errorMessage ->
                    Html.div []
                        [ Html.span []
                            [ Html.text errorMessage ]
                        , Html.input
                            [ Html.Events.onClick msg
                            , Html.Attributes.value value
                            , Html.Attributes.disabled meta.disabled
                            ]
                            []
                        ]
            }


# Base

@docs Field, Metadata


# Viewing fields

@docs ViewConfig, view


# Interacting with fields

@docs init, resetValue, extractValue, resetMetadata, extractMetadata, toMaybe, toResult, withDefault, isValid, isInvalid


# Validation

@docs ValidationFunc, test, noValidation

-}

import Html exposing (Html)


{-| The field type, it represents all the possible state that a field
can be in. It has take parameters of an error type and a value type.

Unless you're trying to model some unique data you probably won't be using this
type, but one with the `value` and `error` arguements already applied. Take a look at
at [`Field.String`](#Field-String).

-}
type Field value error
    = Field value Metadata (Status error)


type Status error
    = Valid
    | Invalid error


{-| A type to reperesent various bits of data about any individiual field. You can get this recode
from a field with [`extractMetadata`](#extractMetadata), and set this record with [`resetMetadata`](#resetMetadata)
-}
type alias Metadata =
    { touched : Bool
    , active : Bool
    , disabled : Bool
    }


{-| A mapping from different field statuses to `Html`
-}
type alias ViewConfig value error msg =
    { valid : Metadata -> value -> Html msg
    , invalid : Metadata -> value -> error -> Html msg
    }


{-| Takes a `ViewConfig` and a field and uses the different possible `Html` based on the field's status
-}
view : ViewConfig value error msg -> Field value error -> Html msg
view viewMap (Field value meta status) =
    case status of
        Valid ->
            viewMap.valid meta value

        Invalid error ->
            viewMap.invalid meta value error


{-| Create a new field with the given value that is in a valid status
-}
init : value -> Field value error
init value =
    Field value { touched = False, active = False, disabled = False } Valid


{-| Reset a field with a new value, _and_ set it to the valid status
-}
resetValue : Field value error -> value -> Field value error
resetValue ((Field _ meta _) as field) newValue =
    Field newValue meta Valid


{-| Extract the metadata from a field, regardless of the field's status
-}
extractValue : Field value error -> value
extractValue (Field value _ _) =
    value


{-| Reset a field with new metadata
-}
resetMetadata : Field value error -> Metadata -> Field value error
resetMetadata ((Field value _ status) as field) newMetadata =
    Field value newMetadata status


{-| Extract the value from a field, regardless of the field's status
-}
extractMetadata : Field value error -> Metadata
extractMetadata (Field _ metadata _) =
    metadata


{-| Convert a field to a `Maybe`. This discards the `error`.
-}
toMaybe : Field value error -> Maybe value
toMaybe (Field value _ status) =
    case status of
        Valid ->
            Just value

        Invalid _ ->
            Nothing


{-| Convert a field to a `Result`
-}
toResult : Field value error -> Result error value
toResult (Field value _ status) =
    case status of
        Valid ->
            Ok value

        Invalid error ->
            Err error


{-| Return the value of a field if it is in a valid status, otherwise get the default value provided
-}
withDefault : value -> Field value error -> value
withDefault default (Field value _ status) =
    case status of
        Valid ->
            value

        Invalid _ ->
            default


{-| Returns true if the field in currently in a valid state, false otherwise
-}
isValid : Field value error -> Bool
isValid (Field value meta status) =
    case status of
        Valid ->
            True

        Invalid _ ->
            False


{-| Returns true if the field in currently in a invalid state, false otherwise
-}
isInvalid : Field value error -> Bool
isInvalid (Field value meta status) =
    case status of
        Invalid _ ->
            True

        _ ->
            False



-- BASE VALIDATOR


{-| Type alias that takes a field, and returns a field

This is the same type as a what a validation function will return

-}
type alias ValidationFunc value error =
    Field value error -> Field value error


{-| Test a valid field against the provided function. If it passes then return the field the exact same,
otherwise return a field marked as invalid with the provided error. If the field is already invalid,
then this function just returns the field as it got it. This is to keep the exisitng error, so you can
chain together validation functions easily.

Look to the [`String`](#String) and [`Number`](#Number) modules for pre-created
validation functions.

-}
test : (value -> Bool) -> error -> ValidationFunc value error
test predicate error ((Field value meta status) as field) =
    case status of
        Valid ->
            if predicate value then
                field
            else
                Field value meta (Invalid error)

        Invalid _ ->
            field


{-| This is an alias for identity, and reperesents an noop validation function.
This may seem odd, but can be useful when working with optional fields
-}
noValidation : ValidationFunc value error
noValidation =
    identity
