module Field.String
    exposing
        ( Field
        , ValidationFunc
        , atLeast
        , atMost
        , email
        , exactly
        , notEmpty
        , optional
        )

{-| A pre-applied `String` version of the `Field` type, and validation function
to go along with them.


# Base

@docs Field, ValidationFunc


# Validation

@docs notEmpty, email, atLeast, atMost, exactly, optional

-}

import Char
import Field as F exposing (Field)
import Parser as P exposing ((|.), (|=))


{-| A field to hold a `String` value, with an error type of `String`
-}
type alias Field =
    F.Field String String


{-| A validation function for a string `Field`
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

    [ >1 upper,lower,digit,symbol ]@[ >2 upper,lower,digit,symbol ].[ >2 upper,lower ]

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

                Err err ->
                    let
                        f =
                            Debug.log "error" err
                    in
                    False
        )
        "Invalid email"


emailParser : P.Parser String
emailParser =
    P.succeed (\main at domain dot -> main ++ at ++ domain ++ toString dot)
        |= P.keep (P.AtLeast 2) (\c -> isAlphaNum c || isSymbol c)
        |= P.keep (P.Exactly 1) (\c -> c == '@')
        |= P.keep (P.AtLeast 2) (\c -> isAlphaNum c || isSymbolWithoutPeriod c)
        |= P.symbol "."
        |. P.end


isAlphaNum : Char -> Bool
isAlphaNum c =
    Char.isLower c || Char.isUpper c || Char.isDigit c


isSymbolWithoutPeriod : Char -> Bool
isSymbolWithoutPeriod c =
    case c of
        ',' ->
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
    if isSymbol c then
        True
    else if c == '.' then
        True
    else
        False


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
