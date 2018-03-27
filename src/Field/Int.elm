module Field.Int
    exposing
        ( IntField
        , IntValidationFunc
        , atLeast
        , atMost
        , greaterThan
        , lessThan
        )

{-| A pre-applied `Int` version of the `Field` type, and validation function
to go along with them.


# Base

@docs IntField, IntValidationFunc


# Validation

@docs atLeast, atMost, greaterThan, lessThan

-}

import Field as F exposing (Field)


{-| A field to hold a `String` value, with an error type of `String`
-}
type alias IntField =
    Field String Int


{-| A validation function for a `StringField`
-}
type alias IntValidationFunc =
    F.ValidationFunc String Int


{-| Enforce that the a field is at least `x` digits long
-}
atLeast : Int -> IntValidationFunc
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
atMost : Int -> IntValidationFunc
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
greaterThan : Int -> IntValidationFunc
greaterThan x =
    F.test
        (\value -> value > x)
        ("Must be at greater than " ++ toString x)


{-| Enforce that a field is greater than `x`
-}
lessThan : Int -> IntValidationFunc
lessThan x =
    F.test
        (\value -> value < x)
        ("Must be at less than " ++ toString x)
