module Field.String exposing
    ( Field, ViewConfig, ValidationFunc
    , notEmpty, email, numeric, nonnumeric, atLeast, atMost, exactly, optional
    )

{-| A pre-applied `String` version of the `Field` type, and validation function
to go along with them.


# Base

@docs Field, ViewConfig, ValidationFunc


# Validation

@docs notEmpty, email, numeric, nonnumeric, atLeast, atMost, exactly, optional

-}

import Char
import Field as F exposing (Field)
import Field.String.Helpers as H
import Parser as P


{-| A field to hold a `String` value, with an error type of `String`. See [`Field`](#Field)
-}
type alias Field =
    F.Field String String


{-| A view config object for String fields. See [`ViewConfig`](#ViewConfig)
-}
type alias ViewConfig msg =
    F.ViewConfig String String msg


{-| A validation function for a String `Field`
-}
type alias ValidationFunc =
    F.ValidationFunc String String


{-| Enforces that a field is not empty
-}
notEmpty : ValidationFunc
notEmpty =
    F.createValidator ((/=) "") "Can't be empty"


{-| Enforce that a field is an email.

The format the email just follow is:

    [ >1 upper,lower,digit,symbol ]@[ >2 upper,lower,digit,symbol ].[ >2 upper,lower,digit, ]

To validate emails, we don't use regex we use
[elm-tools/parser](https://github.com/elm-tools/parser) to validate. If
there's a specific format you need your emails to follow, you can easily implement your
own validation function.

-}
email : ValidationFunc
email =
    F.createValidator
        (\value ->
            value
                |> P.run H.emailParser
                |> H.resultToBool
        )
        "Invalid email"


{-| Enforce that a field contains only numbers.
-}
numeric : ValidationFunc
numeric =
    F.createValidator
        (\value ->
            value
                |> P.run H.numericParser
                |> H.resultToBool
        )
        "Must be numeric"


{-| Enforce that a field does not contains only numbers.
-}
nonnumeric : ValidationFunc
nonnumeric =
    F.createValidator
        (\value ->
            value
                |> P.run H.nonnumericParser
                |> H.resultToBool
        )
        "Must be nonnumeric"


{-| Enforce that a field is at least `x` characters long
-}
atLeast : Int -> ValidationFunc
atLeast x =
    F.createValidator (\value -> String.length value >= x)
        ("Must be at least " ++ String.fromInt x ++ " characters")


{-| Enforce that a field is at most `x` characters long
-}
atMost : Int -> ValidationFunc
atMost x =
    F.createValidator (\value -> String.length value <= x)
        ("Must be at most " ++ String.fromInt x ++ " characters")


{-| Enforce that a field is exactly `x` characters long
-}
exactly : Int -> ValidationFunc
exactly x =
    F.createValidator (\value -> String.length value <= x)
        ("Must be at exactly " ++ String.fromInt x ++ " characters")


{-| A validation function wrapper that will only run the `ValidationFunc` if the provided
if the field's value is not `""`. If the field's value is `""` then this will just return the
field
-}
optional : ValidationFunc -> ValidationFunc
optional validationFunction field =
    case F.extractValue field of
        "" ->
            field

        _ ->
            validationFunction field
