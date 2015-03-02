{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Language.Swift.Lexer where

import Control.Monad.Reader
import Text.ParserCombinators.Parsec
import qualified Text.Parsec.Token as P

type LanguageDef st env = P.GenLanguageDef String st (ReaderT env IO)

swiftIdl :: LanguageDef st env
swiftIdl = P.LanguageDef
    { P.commentStart    = "/*"
    , P.commentEnd      = "*/"
    , P.commentLine     = "//"
    , P.nestedComments  = True
    , P.identStart      = letter <|> oneOf "#$&*+-./:<=>?^~_"
    , P.identLetter     = alphaNum <|> oneOf "#$&*+-./:<=>?^~_"
    , P.opStart         = P.identStart swiftIdl
    , P.opLetter        = P.identStart swiftIdl
    , P.reservedNames   = 
            [ "class" 
            , "deinit"
            , "enum"
            , "extension"
            , "func"
            , "import"
            , "init" 
            , "internal"
            , "let"
            , "operator"
            , "private"
            , "protocol"
            , "public"
            , "static"
            , "struct"
            , "subscript"
            , "typealias"
            , "var"
            , "break"
            , "case"
            , "continue"
            , "default"
            , "do"
            , "else"
            , "fallthrough"
            , "for"
            , "if"
            , "in"
            , "return"
            , "switch"
            , "where"
            , "while"
            , "as"
            , "dynamicType"
            , "false"
            , "is"
            , "nil"
            , "self"
            , "Self"
            , "super"
            , "true"
            , "__COLUMN__"
            , "__LINE__"
            , "__FILE__"
            , "__FUNCTION__"
            ]
    , P.reservedOpNames = ["=", ".", "->"]
    , P.caseSensitive   = True
    }

lexer       = P.makeTokenParser swiftIdl

angles      = P.angles lexer
braces      = P.braces lexer
brackets    = P.brackets lexer
colon       = P.colon lexer
comma       = P.comma lexer
commaSep1   = P.commaSep1 lexer
decimal     = P.decimal lexer
identifier  = P.identifier lexer
integer     = P.integer lexer
keyword     = P.reserved lexer
lexeme      = P.lexeme lexer
natural     = P.natural lexer
parens      = P.parens lexer
semi        = P.semi lexer
semiSep     = P.semiSep lexer
symbol      = P.symbol lexer
whiteSpace  = P.whiteSpace lexer

equal       = symbol "="
semiEnd p   = endBy p semi
commaEnd p  = endBy p comma
commaEnd1 p = endBy1 p comma

reserved = P.reserved lexer
reservedOp = P.reservedOp lexer

semiOrComma = semi <|> comma

semiOrCommaSep p     = sepBy p semiOrComma
semiOrCommaSep1 p    = sepBy1 p semiOrComma
semiOrCommaEnd p     = endBy p semiOrComma
semiOrCommaSepEnd p  = sepEndBy p semiOrComma
semiOrCommaSepEnd1 p = sepEndBy1 p semiOrComma

arrowSep1 p = sepBy1 p $ reservedOp "->"

quote = symbol "\""
quotes = between quote quote

stringLiteral = P.stringLiteral lexer

unescapedStringLiteral = quotes $ many $ satisfy (/= '"')

float = do
    s <- sign
    f <- P.float lexer
    return $ s f
        where
            sign = (char '-' >> return negate)
                <|> (char '+' >> return id)
                <|> return id


