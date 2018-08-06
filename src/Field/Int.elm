module Field.Int
    exposing
        ( Field
        , ViewConfig
        , ValidationFunc
        , atLeast
        , atMost
        , greaterThan
        , greaterThanOrEqual
        , lessThan
        , lessThanOrEqual
        )

{-| A pre-applied `Int` version of the `Field` type, and validation functions
to go along with them.


# Base

@docs Field, ViewConfig, ValidationFunc


# Validation

@docs atLeast, atMost, greaterThan, greaterThanOrEqual, lessThan, lessThanOrEqual

-}

import Field as F exposing (Field)


{-| A field to hold a `Int` value, with an error type of `String`. See [`Field`](#Field)
-}
type alias Field =
    F.Field Int String


{-| A view config object for Int fields. See [`ViewConfig`](#ViewConfig)
-}
type alias ViewConfig msg =
    F.ViewConfig Int String msg


{-| A validation function for an Int `Field`
-}
type alias ValidationFunc =
    F.ValidationFunc Int String


{-| Enforce that the a field is at least `x` digits long
-}
atLeast : Int -> ValidationFunc
atLeast x =
    F.test
        (\value ->
            value
                |> toString
                |> String.length
                |> (\v -> v >= x)
        )
        ("Must be at least " ++ toString x ++ "digits")


{-| Enforce that a field is at most `x` digits long
-}
atMost : Int -> ValidationFunc
atMost x =
    F.test
        (\value ->
            value
                |> toString
                |> String.length
                |> (\v -> v <= x)
        )
        ("Must be at least " ++ toString x ++ "digits")


{-| Enforce that a field is greater than `x`
-}
greaterThan : Int -> ValidationFunc
greaterThan x =
    F.test
        (\value -> value > x)
        ("Must be at greater than " ++ toString x)


{-| Enforce that a field is greater than or equal to `x`
-}
greaterThanOrEqual : Int -> ValidationFunc
greaterThanOrEqual x =
    F.test
        (\value -> value >= x)
        ("Must be at greater than or equal to " ++ toString x)


{-| Enforce that a field is less than `x`
-}
lessThan : Int -> ValidationFunc
lessThan x =
    F.test
        (\value -> value < x)
        ("Must be at less than " ++ toString x)


{-| Enforce that a field is less than or equal to `x`
-}
lessThanOrEqual : Int -> ValidationFunc
lessThanOrEqual x =
    F.test
        (\value -> value <= x)
        ("Must be at less than or equal to " ++ toString x)
