module Field.String.Helpers exposing (atLeast, emailParser, isAlphaNum, isSymbol, isSymbolWithoutPeriod, nonnumericParser, numericParser)

import Char
import Parser as P exposing ((|.), (|=))



-- PARSER HELPERS


atLeast : Int -> (Char -> Bool) -> P.Parser ()
atLeast number pred =
    P.succeed ()
        |. P.chompIf pred
        |. (if number > 1 then
                atLeast (number - 1) pred

            else
                P.chompWhile pred
           )



-- EMAIL


emailParser : P.Parser ()
emailParser =
    P.succeed ()
        |. atLeast 2 (\c -> isAlphaNum c || isSymbol c)
        |. P.symbol "@"
        |. atLeast 2 (\c -> isAlphaNum c || isSymbolWithoutPeriod c)
        |. P.symbol "."
        |. atLeast 2 (\c -> isAlphaNum c)
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
    if c == '.' then
        True

    else
        isSymbolWithoutPeriod c



-- NUMERIC


numericParser : P.Parser ()
numericParser =
    P.succeed ()
        |. atLeast 0 Char.isDigit
        |. P.end



-- NON NUMERIC


nonnumericParser : P.Parser ()
nonnumericParser =
    P.succeed ()
        |. atLeast 0 (Char.isDigit >> not)
        |. P.end
