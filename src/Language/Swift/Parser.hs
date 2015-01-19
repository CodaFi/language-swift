module Language.Swift.Parser where

import Data.List
import Data.Function

import Control.Applicative
import Control.Monad.Reader

import Text.Parsec hiding (many, optional, (<|>))

import System.Directory
import System.FilePath
import System.IO

import Language.Swift.Lexer
import Language.Swift.Schema

type Symbols = [Declaration]

data Environment = Environment
    { currentNamespaces :: [Namespace]  -- namespace(s) in current context
    , currentParams :: [TypeParam]      -- type parameter(s) for current type (struct or alias)
    , currentFile :: FilePath           -- path of the current file
    }

newEnvironment :: FilePath -> Environment
newEnvironment = Environment [] []

type Parser a = ParsecT String Symbols (ReaderT Environment IO) a

parseFile :: FilePath -> IO (Either ParseError Swift)
parseFile file = do
    cwd <- getCurrentDirectory
    h <- openFile file ReadMode
    hSetEncoding h utf8_bom
    input <- hGetContents h
    runReaderT (parseSwift file input) (newEnvironment (cwd </> file))

parseSwift :: SourceName -> String -> ReaderT Environment IO (Either ParseError Swift)
parseSwift = runParserT swift []

data Swift = Swift [Import] [Declaration] deriving Show

swift :: Parser Swift
swift = do
    whiteSpace
    Swift [] <$> many declaration <* eof

declaration :: Parser Declaration
declaration = try structDeclaration
          <|> try constantDeclaration
         -- <|> try variableDeclaration
          <|> try typealiasDeclaration
          <|> try functionDeclaration
         -- <|> try enumDeclaration
         -- <|> try classDeclaration
         -- <|> try protocolDeclaration
         -- <|> try initializerDeclaration
         -- <|> try deinitializeDeclaration
         -- <|> try extensionDeclaration

parameters :: Parser [TypeParam]
parameters = option [] (angles $ commaSep1 $ TypeParam <$> identifier) <?> "type parameters"

typeDeclaration :: Parser Type
typeDeclaration = try userTypeDeclaration
              <|> try dictionaryTypeDeclaration
              <|> try arrayTypeDeclaration
        where
            arrayTypeDeclaration = do 
                t <- brackets typeDeclaration
                optionals <- many (string "?" <|> string "!")
                pure (resolveOptionals optionals $ Array t) <* optional semi <* optional whiteSpace
            dictionaryTypeDeclaration = do
                (k, v) <- (,) <$> (string "[" *> typeDeclaration <* colon) <*> (typeDeclaration <* string "]")
                optionals <- many (string "?" <|> string "!")
                pure (resolveOptionals optionals $ Dictionary k v) <* optional semi <* optional whiteSpace
            userTypeDeclaration = do
                name <- identifier
                params <- parameters
                optionals <- many (string "?" <|> string "!")
                pure (resolveOptionals optionals $ UserType name params) <* optional semi <* optional whiteSpace <?> "type declaration"
                
            resolveOptionals [] t = t
            resolveOptionals (x:xs) t = case x of
                                            "?" -> resolveOptionals xs (Optional t)
                                            "!" -> resolveOptionals xs (ImplicitlyUnwrappedOptional t)
                                            _   -> t

accessLevelModifier :: Parser AccessLevelModifier
accessLevelModifier = fromMaybe Internal <$> optional (keyword "public" *> pure Public
                                                  <|> keyword "private" *> pure Private
                                                  <|> keyword "internal" *> pure Internal)

structDeclaration :: Parser Declaration
structDeclaration = do
    lvl <- accessLevelModifier
    name <- keyword "struct" *> identifier <?> "struct defintiion"
    params <- parameters
    namespaces <- asks currentNamespaces
    local (with params) $ Struct namespaces name params lvl <$> fields <* optional whiteSpace
        where
            with params e = e { currentParams = params }
            fields = braces $ many declaration

constantDeclaration :: Parser Declaration
constantDeclaration = Constant <$> accessLevelModifier <*> (keyword "let" *> identifier) <*> (colon *> typeDeclaration) <* optional semi
    
typealiasDeclaration :: Parser Declaration
typealiasDeclaration = TypeAlias <$> accessLevelModifier <*> (keyword "typealias" *> identifier <* equal) <*> typeDeclaration

functionDeclaration :: Parser Declaration
functionDeclaration = Function <$> accessLevelModifier <*> (keyword "func" *> identifier) <*> parameters <*> sig
    where
        sig = concat <$> many (parens $ commaSep1 param) <?> "parameter clause"
            where
                modifier = optional (keyword "let" *> pure Let
                                 <|> keyword "inout" *> pure Inout
                                 <|> keyword "#" *> pure Hash)
                param = FunctionParam <$> modifier <*> optional identifier <*> (identifier <|> string "_") <*> (colon *> typeDeclaration)

