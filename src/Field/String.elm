module Field.String
    exposing
        ( Field
        , ValidationFunc
        , ViewConfig
        , atLeast
        , atMost
        , email
        , exactly
        , nonnumeric
        , notEmpty
        , numeric
        , optional
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
import Parser as P exposing ((|.), (|=))


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
    F.test ((/=) "") "Can't be empty"


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
    F.test
        (\value ->
            case P.run emailParser value of
                Ok _ ->
                    True

                Err _ ->
                    False
        )
        "Invalid email"


emailParser : P.Parser ()
emailParser =
    P.succeed ()
        |. P.keep (P.AtLeast 2) (\c -> isAlphaNum c || isSymbol c)
        |. P.keep (P.Exactly 1) (\c -> c == '@')
        |. P.keep (P.AtLeast 2) (\c -> isAlphaNum c || isSymbolWithoutPeriod c)
        |. P.symbol "."
        |. P.keep (P.AtLeast 2) (\c -> isAlphaNum c)
        |. P.end


isAlphaNum : Char -> Bool
isAlphaNum c =
    Char.isLower c || Char.isUpper c || Char.isDigit c


isSymbolWithoutPeriod : Char -> Bool
isSymbolWithoutPeriod c =
    case c of
        ',' ->
            True

        '+' ->
            True

        ';' ->
            True

        '-' ->
            True

        '_' ->
            True

        '=' ->
            True

        _ ->
            False


isSymbol : Char -> Bool
isSymbol c =
    if isSymbolWithoutPeriod c then
        True
    else if c == '.' then
        True
    else
        False


{-| Enforce that a field contains only numbers.
-}
numeric : ValidationFunc
numeric =
    F.test
        (\value ->
            case P.run numericParser value of
                Ok _ ->
                    True

                Err _ ->
                    False
        )
        "Must be numeric"


numericParser : P.Parser ()
numericParser =
    P.succeed ()
        |. P.keep (P.AtLeast 0) Char.isDigit
        |. P.end


{-| Enforce that a field does not contains only numbers.
-}
nonnumeric : ValidationFunc
nonnumeric =
    F.test
        (\value ->
            case P.run nonnumericParser value of
                Ok _ ->
                    True

                Err _ ->
                    False
        )
        "Must be numeric"


nonnumericParser : P.Parser ()
nonnumericParser =
    P.succeed ()
        |. P.keep (P.AtLeast 0) (Char.isDigit >> not)
        |. P.end


{-| Enforce that a field is at least `x` characters long
-}
atLeast : Int -> ValidationFunc
atLeast x =
    F.test (\value -> String.length value >= x)
        ("Must be at least " ++ toString x ++ " characters")


{-| Enforce that a field is at most `x` characters long
-}
atMost : Int -> ValidationFunc
atMost x =
    F.test (\value -> String.length value <= x)
        ("Must be at most " ++ toString x ++ " characters")


{-| Enforce that a field is exactly `x` characters long
-}
exactly : Int -> ValidationFunc
exactly x =
    F.test (\value -> String.length value <= x)
        ("Must be at exactly " ++ toString x ++ " characters")


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
