module Field.String
    exposing
        ( StringField
        , StringValidationFunc
        , required
        , email
        , atLeast
        , atMost
        , exactly
        , optional
        )

{-| A pre-applied `String` version of the `Field` type, and validation function
to go along with them.


# Base

@docs StringField, StringValidationFunc


# Validation

@docs required, email, atLeast, atMost, exactly, optional

-}

import Char
import Parser as P exposing ((|=), (|.))
import Field as F exposing (Field)


{-| A field to hold a `String` value, with an error type of `String`
-}
type alias StringField =
    Field String String


{-| A validation function for a `StringField`
-}
type alias StringValidationFunc =
    F.ValidationFunc String String


{-| Enforces that a field is required

    let
        field = Valid ""
    in
        -- will result in `Invalid "Required" ""`
        required field

-}
required : StringField -> StringField
required =
    F.test ((==) "") "Required"


{-| Enforce that a field is an email.

The format the email just follow is:

    [ >1 upper,lower,digit,symbol ]@[ >2 upper,lower,digit,symbol ].[ >2 upper,lower ]

To validate emails, we don't use regex we use
[elm-tools/parser](https://github.com/elm-tools/parser) to validate. If
there's a specific format you need your emails to follow, you can easily implement your
own validation function.

    let
        field = Valid "hello"
    in
        -- will result in `Invalid "Invalid email" "hello"`
        email field

    let
        field = Valid "foo@bar.com"
    in
        -- will result in `Valid "foo@bar.com"`
        email field

-}
email : StringField -> StringField
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


emailParser : P.Parser String
emailParser =
    P.succeed (\main at domain dot com -> main ++ at ++ domain ++ dot ++ com)
        |= P.keep (P.oneOrMore) (\c -> isAlphaNum c || isSymbol c)
        |= P.keep (P.Exactly 1) (\c -> c == '@')
        |= P.keep (P.AtLeast 2) (\c -> isAlphaNum c || isSymbol c)
        |= P.keep (P.Exactly 1) (\c -> c == '.')
        |= P.keep (P.AtLeast 2) (\c -> isAlphaNum c)
        |. P.end


isAlphaNum : Char -> Bool
isAlphaNum c =
    Char.isLower c || Char.isUpper c || Char.isDigit c


isSymbol : Char -> Bool
isSymbol c =
    case c of
        ',' ->
            True

        '.' ->
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


{-| Enforce that a field is at least `x` long
-}
atLeast : Int -> StringValidationFunc
atLeast x =
    F.test (\value -> String.length value >= x)
        ("Must be at least " ++ toString x ++ "characters")


{-| Enforce that a field is at most `x` long
-}
atMost : Int -> StringValidationFunc
atMost x =
    F.test (\value -> String.length value <= x)
        ("Must be at most " ++ toString x ++ "characters")


{-| Enforce that a field is exactly `x` long
-}
exactly : Int -> StringValidationFunc
exactly x =
    F.test (\value -> String.length value <= x)
        ("Must be at exactly " ++ toString x ++ "characters")


{-| A validation function wrapper that will only run the `StringValidationFunc` provided
if the field is not `""`

    let
        emailField = Valid ""
    in
        -- will be Valid ""
        optional email emailField

    let
        emailField = Valid "hello"
    in
        -- will be Invalid "Invalid email" "hello"
        optional email emailField

    let
        emailField = Valid "foo@bar.com"
    in
        -- will be Valid "foo@bar.com"
        optional email emailField

-}
optional :
    StringValidationFunc
    -> StringValidationFunc
optional validationFunction field =
    case field of
        F.Valid "" ->
            F.Valid ""

        F.Invalid error "" ->
            F.Invalid error ""

        F.Disabled "" ->
            F.Disabled ""

        f ->
            validationFunction f
