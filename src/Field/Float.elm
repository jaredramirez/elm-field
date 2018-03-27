module Field.Float
    exposing
        ( FloatField
        , FloatValidationFunc
        , greaterThan
        , lessThan
        )

{-| A pre-applied `Float` version of the `Field` type, and validation function
to go along with them.


# Base

@docs FloatField, FloatValidationFunc


# Validation

@docs greaterThan, lessThan

-}

import Field as F exposing (Field)


{-| A field to hold a `String` value, with an error type of `String`
-}
type alias FloatField =
    Field String Float


{-| A validation function for a `StringField`
-}
type alias FloatValidationFunc =
    F.ValidationFunc String Float


{-| Enforce that a field is greater than `x`
-}
greaterThan : Float -> FloatValidationFunc
greaterThan x =
    F.test
        (\value -> value > x)
        ("Must be at greater than " ++ toString x)


{-| Enforce that a field is greater than `x`
-}
lessThan : Float -> FloatValidationFunc
lessThan x =
    F.test
        (\value -> value < x)
        ("Must be at less than " ++ toString x)
