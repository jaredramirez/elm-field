module Field.Bool
    exposing
        ( Field
        , ValidationFunc
        , ViewConfig
        , isFalse
        , isTrue
        )

{-| A pre-applied `Bool` version of the `Field` type, and validation functions
to go along with them.


# Base

@docs Field, ViewConfig, ValidationFunc


# Validation

@docs isTrue, isFalse

-}

import Field as F exposing (Field)


{-| A field to hold a `Bool` value, with an error type of `String`. See [`Field`](#Field)
-}
type alias Field =
    F.Field Bool String


{-| A view config object for Bool fields. See [`ViewConfig`](#ViewConfig)
-}
type alias ViewConfig msg =
    F.ViewConfig Bool String msg


{-| A validation function for a Bool `Field`
-}
type alias ValidationFunc =
    F.ValidationFunc Bool String


{-| Enforce that a field is true
-}
isTrue : ValidationFunc
isTrue =
    F.test ((==) True) "Must be true"


{-| Enforce that a field is true
-}
isFalse : ValidationFunc
isFalse =
    F.test ((==) False) "Must be false"
