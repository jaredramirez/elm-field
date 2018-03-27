module Field
    exposing
        ( Field(Valid, Invalid, Disabled)
        , set
        , extract
        , withDefault
        , toMaybe
        , isValid
        , isInvalid
        , isDisabled
        , ValidationFunc
        , sequence
        , test
        , noValidation
        )

{-| This library provides a datatype, and accompaning functions, to easily model input
field data.

To use this data type, let's say that you need to have an email input that is required.

First, you can create a field in your model

    type alias Model =
        { email : StringField }

Then, you a message to set that field to a value

    type Msg
        = SetEmailField String

Next, you add logic to set & validate the field to your update function

    update : Msg -> Model -> Model
    update msg model =
        case msg of
            SetEmailField value ->
                { model
                    | emailField =
                        set model.emailField value
                            |> require
                            |> email
                }

Finally, wire it into the view!

    view : Model -> Html Msg
    view model =
        Html.div []
            [ Html.h1 []
                [ Html.text "Form Example" ]
            , case model.emailField of
                Valid value ->
                    Html.input
                        [ Html.Events.onClick SetEmailField
                        , Html.Attributes.value value
                        ]
                        []

                Invalid errorMessage value ->
                    Html.div []
                        [ Html.span []
                            [ Html.text errorMessage ]
                        , Html.input
                            [ Html.Events.onClick SetEmailField
                            , Html.Attributes.value value
                            ]
                            []
                        ]

                Disabled value ->
                    Html.input
                        [ Html.Attributes.value value
                        , Html.Attributes.disabled True
                        ]
                        []
            ]

To see a complete example that handles multiple fields and does some action on
submit, take a look [at this]()


# Base

@docs Field


# Interacting with fields

@docs set, extract, toMaybe, withDefault, isValid, isInvalid, isDisabled


# Validation

@docs ValidationFunc, test, sequence, noValidation

-}


{-| The field type, it represents all the possible state that a field
can be in. It has take parameters of an error type and a value type.

Unless you're trying to model some unique data you probably won't be using this
type, but a type with these arguements already applied. Take a look at
at [`Field.String`](#Field-String).

-}
type Field err value
    = Valid value
    | Invalid err value
    | Disabled value


{-| Set a field to a new value, unless the field is `Disabled`

    -- will result in `Valid "goodbye"
    set (Valid "hello") "goodbye"

    -- will result in `Valid 11`
    set (Invalid "This is an error message" 10) 11

    -- will result in `Disabled "Ha, ha!"`
    set (Disabled "Ha, ha!") "ABC"

-}
set : Field error value -> value -> Field error value
set field newValue =
    case field of
        Valid _ ->
            Valid newValue

        Invalid _ _ ->
            Valid newValue

        Disabled _ ->
            field


{-| Extract a value from a field, regardless of that field's status
-}
extract : Field error value -> value
extract field =
    case field of
        Valid value ->
            value

        Invalid _ value ->
            value

        Disabled value ->
            value


{-| Convert a `Field` to a `Maybe`

    -- will result in `Just 7`
    toMaybe (Valid 7)

    -- will result in `Nothing`
    toMaybe (Disabled 7)

-}
toMaybe : Field error value -> Maybe value
toMaybe field =
    case field of
        Valid value ->
            Just value

        _ ->
            Nothing


{-| Unwrap the value of a field if it is `Valid`, otherwise get the default value

    -- will result in `"Lamar"`
    withDefault "kendrick" (Valid "Lamar")

    -- will result in `"Cardi"`
    withDefault "Cardi" (Invalid "Error!" "B")

-}
withDefault : value -> Field error value -> value
withDefault default field =
    case field of
        Valid value ->
            value

        _ ->
            default


{-| If the field passed in is `Valid` return `True`, otherwise `False`
-}
isValid : Field error value -> Bool
isValid field =
    case field of
        Valid _ ->
            True

        _ ->
            False


{-| If the field passed in is `Invalid` return `True`, otherwise `False`
-}
isInvalid : Field error value -> Bool
isInvalid field =
    case field of
        Invalid _ _ ->
            True

        _ ->
            False


{-| If the field passed in is `Disabled` return `True`, otherwise `False`
-}
isDisabled : Field error value -> Bool
isDisabled field =
    case field of
        Disabled _ ->
            True

        _ ->
            False



-- BASE VALIDATOR


{-| Type alias that takes a field, and returns a field

This is the same type as a partially applied [`test`](#test), after a validation test and
error message have been applied

-}
type alias ValidationFunc error value =
    Field error value -> Field error value


{-| This is a function to provide an easy way to validate multiple fields.
It takes a list of tuples like `(ValidationFunc, Field)`, and returns a tuple of
whether all the fields are `Valid`, and a list of the validated fields.

It goes through the list provided, and applies each validation function to each field.
Then, if any single field is `Invalid` then returns `(False, fields)` otherwise `(True, fields)`

    let
        emailField =
            Valid "go@gina.com"

        firstNameField =
            Valid "Gina"

        lastNameField =
           Valid ""
    in
        -- will result in
        -- (False, [ Valid "go@gina.com", Valid "Gina", Invalid "Required" ""])
        sequence
            [ (required >> email, emailField)
            , (required, firstNameField)
            , (required, lastNameField)
            ]

-}
sequence :
    List ( ValidationFunc error value, Field error value )
    -> ( Bool, List (Field error value) )
sequence list =
    let
        validatedFields =
            List.map (\( func, field ) -> func field) list

        areFieldsValid =
            List.any (isValid >> not) validatedFields
    in
        ( areFieldsValid, validatedFields )


{-| Test a field against the provided function. If it passes then return the field,
otherwise return an `Invalid` field with the error provided.

Look to the [`String`](#String) and [`Number`](#Number) modules for pre-created
validation functions.

-}
test : (value -> Bool) -> error -> ValidationFunc error value
test predicate error field =
    case field of
        Valid value ->
            if predicate value then
                field
            else
                Invalid error value

        _ ->
            field


{-| This is an alias for identity, and reperesents an noop validation function.
This may seem odd, but can be useful in conjunction with optional fields and
[`sequence`](#sequence)
-}
noValidation : ValidationFunc error value
noValidation =
    identity
