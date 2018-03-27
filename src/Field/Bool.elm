module Field.Bool
    exposing
        ( BoolField
        , BoolValidationFunc
        , isTrue
        , isFalse
        )

{-| A pre-applied `Float` version of the `Field` type, and validation function
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
    Field String Bool


{-| A validation function for a `StringField`
-}
type alias BoolValidationFunc =
    F.ValidationFunc String Bool


{-| Enforce that a field is true
-}
isTrue : BoolValidationFunc
isTrue =
    F.test identity ("Must be true")


{-| Enforce that a field is true
-}
isFalse : BoolValidationFunc
isFalse =
    F.test (identity >> not) ("Must be false")
