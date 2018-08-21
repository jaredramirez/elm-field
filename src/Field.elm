module Field exposing
    ( Field, Metadata
    , ViewConfig, view
    , init, resetValue, extractValue, resetMetadata, extractMetadata, withDefault, toMaybe, toResult, isValid, isInvalid
    , ValidationFunc, createValidator
    )

{-| This library provides a datatype to model and validate input field data.

To use this data type, let's say that you need a sign up form that has a requried name field,
a required email field, and an age field that must be between 18 & 100 that you need to send
to your server after it's validated.

First, you can import the package and create the fields in your model

    ... other imports
    import Field as F
    import Field.Int as FInt
    import Field.String as FStr

    type alias Model =
        { name : FStr.Field
        , email : FStr.Field
        , age : FInt.Field
        }


    init : ( Model, Cmd Msg )
    init =
        { name = F.init ""
        , email = F.init ""
        , age = F.init 0
        }
            ! []

Then, you add a few messages to update the fields, and one to submit your form

    type Msg
        = SetNameField String
        | SetEmailField String
        | SetAgeField String
        | Submit

Next, you add logic to set & validate the fields to your update function

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

            Submit ->
                let
                    name =
                        validateName model.name

                    email =
                        validateEmail model.email

                    age =
                        validateAge model.age

                    cmds =
                        case ( F.toResult name, F.toResult email, F.toResult age ) of
                            ( Ok nameValue, Ok emailValue, Ok ageValue ) ->
                                [ ... some command ... ]

                            _ ->
                                []
                in
                { model
                    | name = name
                    , email = email
                    , age = age
                }
                    ! cmds


    validateName : FStr.ValidationFunc
    validateName =
        FStr.notEmpty


    validateEmail : FStr.ValidationFunc
    validateEmail =
        FStr.notEmpty >> FStr.email


    validateAge : FInt.ValidationFunc
    validateAge =
        FInt.greaterThanOrEqual 18 >> FInt.lessThan 100

Finally, wire it into the view!

    view : Model -> Html Msg
    view model =
        Html.div []
            [ Html.h1 [] [ Html.text "Sign Up!" ]
            , F.view (stringFieldConfig "Name" SetNameField) model.name
            , F.view (stringFieldConfig "Email" SetEmailField) model.email
            , F.view (intFieldConfig "Age" SetAgeField) model.age
            , Html.button [ Html.Events.onClick Submit ]
                [ Html.span [] [ Html.text "Submit" ] ]
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

In this example, we only use the built-in validators, but it's pretty
simple to create your own validators too! Take a look at [an example](https://github.com/jaredramirez/elm-field/blob/master/example/Main.elm) to see a
similar example to the one above but with a custom validator.


# Base

@docs Field, Metadata


# Viewing fields

@docs ViewConfig, view


# Interacting with fields

@docs init, resetValue, extractValue, resetMetadata, extractMetadata, withDefault, toMaybe, toResult, isValid, isInvalid


# Validation

@docs ValidationFunc, createValidator

-}

import Html exposing (Html)


{-| The field type, it represents all the possible state that a field
can be in. It has take parameters of an error type and a value type.

Unless you're trying to model some unique data you probably won't be using this
type, but one with the `value` and `error` arguements already applied. Take a look at the [`Field.String`](#Field-String), [`Field.Int`](#Field-Int) and [`Field.Float`](#Field-Float) modules to see some common types already appplied and for examples to modeling your own data.

-}
type Field value error
    = Field value Metadata (Status error)


type Status error
    = Valid
    | Invalid error


{-| A type to reperesent various bits of data about any individiual field. You can get this record
from a field with [`extractMetadata`](#extractMetadata), and set this record with [`resetMetadata`](#resetMetadata)
-}
type alias Metadata =
    { active : Bool
    , touched : Bool
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


{-| Return the value of a field if it is in a valid status, otherwise get the default value provided
-}
withDefault : value -> Field value error -> value
withDefault default (Field value _ status) =
    case status of
        Valid ->
            value

        Invalid _ ->
            default


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
-}
type alias ValidationFunc value error =
    Field value error -> Field value error


{-| Create a validator by testing a field against the provided function. If the field passes then return the field the exact same,
otherwise return a field marked as invalid with the provided error. If the field is already invalid,
then this function just returns the field as it got it. This is to keep the exisitng error, so you can
chain together validation functions easily.

Look to the [`Field.String`](#Field-String), [`Field.Int`](#Field-Int) and [`Field.Float`](#Field-Float) modules in this package. for pre-created validation functions.

-}
createValidator : (value -> Bool) -> error -> ValidationFunc value error
createValidator predicate error ((Field value meta status) as field) =
    case status of
        Valid ->
            if predicate value then
                field

            else
                Field value meta (Invalid error)

        Invalid _ ->
            field
