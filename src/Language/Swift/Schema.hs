{-# LANGUAGE RecordWildCards #-}

module Language.Swift.Schema where

import Language.Swift.Util
import Data.List

type QualifiedName = [String]

data Type 
    = Tuple [(Maybe String, Type)]
    | Array Type
    | Dictionary Type Type
    | Optional Type | ImplicitlyUnwrappedOptional Type
    | ProtocolType
    | Metatype
    | UserType String [TypeParam]
    deriving Eq
    
instance Show Type where
    show (Tuple ts)                         = "(" ++ intercalate ", " (map showParam ts) ++ ")"
        where
            showParam (mName, t) = maybe "" (++ " : ") mName ++ show t
    show (Array t)                          = "[" ++ show t ++ "]"
    show (Dictionary k v)                   = "[" ++ show k ++ ":" ++ show v ++ "]" 
    show (Optional t)                       = show t ++ "?"
    show (ImplicitlyUnwrappedOptional t)    = show t ++ "!"
    show ProtocolType                       = "Protocol"
    show Metatype                           = "Metatype"
    show (UserType n params)                = n ++ showGenericParams params
      where
        showGenericParams [] = ""
        showGenericParams ps = ("<" ++ intercalate ", " (map show ps) ++ ">")

newtype TypeParam = TypeParam String
    deriving Eq

instance Show TypeParam where
    show (TypeParam x) = x 

data EnumCase = EnumCase
    { caseName :: String
    , caseParams :: [(Maybe String, Type)]
    } deriving Eq

instance Show EnumCase where
    show (EnumCase nm ps) = "case " ++ nm ++ intercalate ", " (map showParam ps) 
        where
            showParam (mName, t) = maybe "" (++ " : ") mName ++ show t

data ParameterModifier = Let | Inout | Hash
    deriving Eq

data FunctionParam = FunctionParam
    { modifier :: Maybe ParameterModifier
    , externalParamName :: Maybe String
    , localParamName :: String
    , paramType :: Type
    } deriving Eq

data Attribute = Attribute                                                                                                                      
    { attrName :: QualifiedName
    , attrValue :: String
    } deriving Eq

data AccessLevelModifier = Internal | Private | Public
    deriving (Eq, Show)

data Declaration 
    = Function
        { accessLevel :: AccessLevelModifier
        , declName :: String
        , declParams :: [TypeParam]
        , signature :: [FunctionParam]
        }
    | Constant
        { accessLevel :: AccessLevelModifier
        , declName :: String
        , constType :: Type
        }
    | Struct
        { declNamespaces :: [Namespace]     -- namespace(s) in which the struct is declared
        , declName :: String                -- struct identifier
        , declParams :: [TypeParam]         -- type parameters for generics
        , accessLevel :: AccessLevelModifier  -- public, private, internal
        , structFields :: [Declaration]     -- zero or more fields
        }
    | Enum
        { accessLevel :: AccessLevelModifier
        , declName :: String                -- enum identifier
        , declParams :: [TypeParam]         -- type parameters for generics
        , enumCases :: [EnumCase]           
        }
    | Protocol
        { accessLevel :: AccessLevelModifier
        , declName :: String 
        , protocolFields :: [ProtocolDeclaration]
        }
    | TypeAlias
        { accessLevel :: AccessLevelModifier
        , declName :: String                -- alias identifier
        , aliasType :: Type                 -- aliased type
        } deriving Eq

data ProtocolDeclaration = ProtocolProperty
    { propertyDeclName :: String
    , propType :: Type
    , hasGetter :: Bool
    , hasSetter :: Bool
    } deriving Eq

showTypeParams :: [TypeParam] -> String
showTypeParams = angles . sepBy ", " show

instance Show Declaration where
    show Function {..} = "func " ++ declName
    show Constant {..} = "let " ++ declName ++ " : " ++ show constType
    show Struct {..} = "struct " ++ declName ++ showTypeParams declParams ++ "{ " ++ show structFields ++ " }"
    show Enum {..} = "enum " ++ declName ++ "{" ++ show enumCases ++ "}"
    show Protocol {..} = undefined
    show TypeAlias {..} = "typealias " ++ declName ++ " = " ++ show aliasType

newtype Namespace = Namespace QualifiedName
    deriving Eq

data Import = Import FilePath

instance Show Import where
    show (Import p) = "import " ++ p

