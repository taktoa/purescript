-- |
-- Metadata annotations for core functional representation
--
module Language.PureScript.CoreFn.Meta where

import Prelude.Compat

import Control.Applicative

import Data.Aeson
import qualified Data.Text as T

import Language.PureScript.Names

-- |
-- Metadata annotations
--
data Meta
  -- |
  -- The contained value is a data constructor
  --
  = IsConstructor ConstructorType [Ident]
  -- |
  -- The contained value is a newtype
  --
  | IsNewtype
  -- |
  -- The contained value is a typeclass dictionary constructor
  --
  | IsTypeClassConstructor
  -- |
  -- The contained reference is for a foreign member
  --
  | IsForeign deriving (Show, Eq)

instance ToJSON Meta where
  toJSON (IsConstructor ct as)  = toJSON (T.pack "IsConstructor", ct, as)
  toJSON IsNewtype              = toJSON $ T.pack "IsNewtype"
  toJSON IsTypeClassConstructor = toJSON $ T.pack "IsTCConstructor"
  toJSON IsForeign              = toJSON $ T.pack "IsForeign"

instance FromJSON Meta where
  parseJSON (String "IsNewtype")       = pure IsNewtype
  parseJSON (String "IsTCConstructor") = pure IsTypeClassConstructor
  parseJSON (String "IsForeign")       = pure IsForeign
  parseJSON owise                      = do
    tuple <- parseJSON owise
    let ic = T.pack "IsConstructor"
    case tuple of (tag, ct, as) | tag == ic -> pure $ IsConstructor ct as
                  _                         -> empty

-- |
-- Data constructor metadata
--
data ConstructorType
  -- |
  -- The constructor is for a type with a single construcor
  --
  = ProductType
  -- |
  -- The constructor is for a type with multiple construcors
  --
  | SumType deriving (Show, Eq)

instance ToJSON ConstructorType where
  toJSON ProductType = "Product"
  toJSON SumType     = "Sum"

instance FromJSON ConstructorType where
  parseJSON (String "Product") = pure ProductType
  parseJSON (String "Sum")     = pure SumType
  parseJSON _                  = empty
