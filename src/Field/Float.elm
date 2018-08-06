module Field.Float
    exposing
        ( Field
        , ValidationFunc
        , ViewConfig
        , greaterThan
        , lessThan
        )

{-| A pre-applied `Float` version of the `Field` type, and validation functions
to go along with them.


# Base

@docs Field, ViewConfig, ValidationFunc


# Validation

@docs greaterThan, lessThan

-}

import Field as F


{-| A field to hold a `Float` value, with an error type of `String`. See [`Field`](#Field)
-}
type alias Field =
    F.Field Float String


{-| A view config object for Float fields. See [`ViewConfig`](#ViewConfig)
-}
type alias ViewConfig msg =
    F.ViewConfig Float String msg


{-| A validation function for a Float `Field`
-}
type alias ValidationFunc =
    F.ValidationFunc Float String


{-| Enforce that a field is greater than `x`
-}
greaterThan : Float -> ValidationFunc
greaterThan x =
    F.test
        (\value -> value > x)
        ("Must be at greater than " ++ toString x)


{-| Enforce that a field is greater than `x`
-}
lessThan : Float -> ValidationFunc
lessThan x =
    F.test
        (\value -> value < x)
        ("Must be at less than " ++ toString x)
