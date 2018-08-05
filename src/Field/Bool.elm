module Field.Bool
    exposing
        ( BoolField
        , BoolValidationFunc
        , isFalse
        , isTrue
        )

{-| A pre-applied `Bool` version of the `Field` type, and validation functions
to go along with them.


# Base

@docs BoolField, BoolValidationFunc


# Validation

@docs isTrue, isFalse

-}

import Field as F exposing (Field)


{-| A field to hold a `String` value, with an error type of `String`
-}
type alias BoolField =
    Field Bool String


{-| A validation function for a Bool `Field`
-}
type alias BoolValidationFunc =
    F.ValidationFunc Bool String


{-| Enforce that a field is true
-}
isTrue : BoolValidationFunc
isTrue =
    F.test identity "Must be true"


{-| Enforce that a field is true
-}
isFalse : BoolValidationFunc
isFalse =
    F.test (identity >> not) "Must be false"
