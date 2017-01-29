{-# LANGUAGE NoOverloadedStrings #-}
-- |
-- Dump the core functional representation in JSON format for consumption
-- by third-party code generators
--
module Language.PureScript.CoreFn.ToJSON
  ( moduleToJSON
  , AnnRenderer
  , defaultAnnRenderer
  , newAnnRenderer
  ) where

import Prelude.Compat

import Control.Arrow ((***))

import Data.Maybe (fromMaybe)
import Data.Aeson
import Data.Version (Version, showVersion)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.HashMap.Lazy as LHM

import Language.PureScript.CoreFn.Ann
import Language.PureScript.AST.Literals
import Language.PureScript.Comments
import Language.PureScript.CoreFn
import Language.PureScript.Names
import Language.PureScript.PSString (PSString, decodeString)

moduleToJSON :: (ToJSON ann) => Version -> Module ann -> Value
moduleToJSON ver mod = insertObject (T.pack "builtWith", toJSON (showVersion ver))
                       $ toJSON mod

insertObject :: (Text, Value) -> Value -> Value
insertObject (k, v) (Object o) = Object $ LHM.insert k v o
insertObject _      value      = value

type AnnRenderer a = a -> Value -> Value

defaultAnnRenderer :: AnnRenderer a
defaultAnnRenderer = const id

newAnnRenderer :: AnnRenderer Ann
newAnnRenderer ann v = object [ T.pack "ann" .= annToJSON ann
                              , T.pack "val" .= v ]

annToJSON :: Ann -> Value
annToJSON (mss, cs, mt, mm) = toJSON (sourceSpanJ, commentsJ, typeJ, metaJ)
  where
    sourceSpanJ = toJSON mss
    commentsJ   = toJSON cs
    typeJ       = toJSON mt
    metaJ       = toJSON (metaToJSON <$> mm)

metaToJSON :: Meta -> Value
metaToJSON = go
  where
    go (IsConstructor ct as)  = toJSON ("Constructor", ctToJSON ct, as)
    go IsNewtype              = toJSON "Newtype"
    go IsTypeClassConstructor = toJSON "TCConstructor"
    go IsForeign              = toJSON "Foreign"
    ctToJSON ProductType = "Product"
    ctToJSON SumType     = "Sum"

{-

type AnnRenderer a = a -> Value -> Value
-- type Ann = (Maybe SourceSpan, [Comment], Maybe Type, Maybe Meta)
defaultAnnRenderer :: AnnRenderer a
defaultAnnRenderer = const id

newAnnRenderer :: AnnRenderer Ann
newAnnRenderer ann v = [ T.pack "ann" .= annToJSON ann
                       , T.pack "val" .= v
                       ] |> object

modulesToJSON :: AnnRenderer a -> [(Version, Module a)] -> Value
modulesToJSON annR = object . map (\(v, m) -> moduleToName m .= moduleJ v m)
  where
    moduleJ = moduleToJSON annR
    moduleToName = runModuleName . moduleName

moduleToJSON :: AnnRenderer a -> Version -> Module a -> Value
moduleToJSON annR v m = [ T.pack "comments"  .= commentsJ
                        , T.pack "imports"   .= importsJ
                        , T.pack "exports"   .= exportsJ
                        , T.pack "foreign"   .= foreignJ
                        , T.pack "decls"     .= declsJ
                        , T.pack "builtWith" .= versionJ
                        ] |> object
  where
    commentsJ = map commentToJSON            (moduleComments m)
    importsJ  = map pairToJSON               (moduleImports m)
    exportsJ  = map identToJSON              (moduleExports m)
    foreignJ  = map foreignToJSON            (moduleForeign m)
    declsJ    = map (bindToJSON annR)        (moduleDecls m)
    versionJ  = toJSON (showVersion v)
    foreignToJSON (i, t) = (identToJSON i, toJSON t)
    pairToJSON (a, mn) = annR a $ moduleNameToJSON mn

bindToJSON :: AnnRenderer a -> Bind a -> Value
bindToJSON annR = object . go
  where
    go (NonRec a n e) = [ runIdent n .= annR a (exprJ e) ]
    go (Rec bs)       = map (\((a, n), e) -> runIdent n .= annR a (exprJ e)) bs
    exprJ = exprToJSON annR

exprToJSON :: AnnRenderer a -> Expr a -> Value
exprToJSON annR = go
  where
    go (Var a i)              = ( "Var"
                                , qualifiedToJSON runIdent i
                                ) |> toJSON |> annR a
    go (Literal a l)          = ( "Literal"
                                , literalToJSON go l
                                ) |> toJSON |> annR a
    go (Constructor a d c is) = ( "Constructor"
                                , properNameToJSON d
                                , properNameToJSON c
                                , map identToJSON is
                                ) |> toJSON |> annR a
    go (Accessor a f r)       = ( "Accessor"
                                , f
                                , go r
                                ) |> toJSON |> annR a
    go (ObjectUpdate a r fs)  = ( "ObjectUpdate"
                                , go r
                                , recordToJSON go fs
                                ) |> toJSON |> annR a
    go (Abs a p b)            = ( "Abs"
                                , identToJSON p
                                , go b
                                ) |> toJSON |> annR a
    go (App a f x)            = ( "App"
                                , go f
                                , go x
                                ) |> toJSON |> annR a
    go (Case a ss cs)         = ( "Case"
                                , map go ss
                                , map (caseAlternativeToJSON annR) cs
                                ) |> toJSON |> annR a
    go (Let a bs e)           = ( "Let"
                                , map (bindToJSON annR) bs
                                , go e
                                ) |> toJSON |> annR a

binderToJSON :: AnnRenderer a -> Binder a -> Value
binderToJSON annR = go
  where
    go (NullBinder a)               = "NullBinder" |> toJSON |> annR a
    go (VarBinder a v)              = ( "VarBinder"
                                      , identToJSON v
                                      ) |> toJSON |> annR a
    go (LiteralBinder a l)          = ( "LiteralBinder"
                                      , literalToJSON go l
                                      ) |> toJSON |> annR a
    go (ConstructorBinder a d c bs) = ( "ConstructorBinder"
                                      , qualifiedToJSON runProperName d
                                      , qualifiedToJSON runProperName c
                                      , map go bs
                                      ) |> toJSON |> annR a
    go (NamedBinder a n b)          = ( "NamedBinder"
                                      , identToJSON n
                                      , go b
                                      ) |> toJSON |> annR a

-- If all of the labels in the record can safely be converted to JSON strings,
-- we generate a JSON object. Otherwise the labels must be represented as
-- arrays of integers in the JSON, and in this case we generate the record as
-- an array of pairs.
recordToJSON :: (a -> Value) -> [(PSString, a)] -> Value
recordToJSON f record = fromMaybe (asArrayOfPairs record) (asObject record)
  where
  asObject = fmap object . traverse (uncurry maybePair)
  maybePair label a = fmap (\l -> l .= f a) (decodeString label)

  asArrayOfPairs = toJSON . map (toJSON *** f)

caseAlternativeToJSON :: AnnRenderer a -> CaseAlternative a -> Value
caseAlternativeToJSON annR (CaseAlternative bs r) = toJSON [bindersJ, resultJ]
  where
    bindersJ = toJSON (map (binderToJSON annR) bs)
    resultJ = either guardsJ simpleJ r
    guardsJ = toJSON . map (exprJ *** exprJ)
    simpleJ = exprJ
    exprJ = exprToJSON annR

literalToJSON :: (a -> Value) -> Literal a -> Value
literalToJSON t = go
  where
    go (NumericLiteral  (Left n)) = toJSON ("IntLiteral", n)
    go (NumericLiteral (Right n)) = toJSON ("NumberLiteral", n)
    go (StringLiteral          s) = toJSON ("StringLiteral", s)
    go (CharLiteral            c) = toJSON ("CharLiteral", c)
    go (BooleanLiteral         b) = toJSON ("BooleanLiteral", b)
    go (ArrayLiteral          xs) = toJSON ("ArrayLiteral", map t xs)
    go (ObjectLiteral         xs) = toJSON ("ObjectLiteral", recordToJSON t xs)

annToJSON :: Ann -> Value
annToJSON (mss, cs, mt, mm) = toJSON (sourceSpanJ, commentsJ, typeJ, metaJ)
  where
    sourceSpanJ = toJSON mss
    commentsJ   = toJSON cs
    typeJ       = toJSON mt
    metaJ       = toJSON (metaToJSON <$> mm)

metaToJSON :: Meta -> Value
metaToJSON = go
  where
    go (IsConstructor ct as)  = toJSON ("Constructor", ctToJSON ct, as)
    go IsNewtype              = toJSON "Newtype"
    go IsTypeClassConstructor = toJSON "TCConstructor"
    go IsForeign              = toJSON "Foreign"
    ctToJSON ProductType = "Product"
    ctToJSON SumType     = "Sum"

identToJSON :: Ident -> Value
identToJSON = toJSON . runIdent

properNameToJSON :: ProperName a -> Value
properNameToJSON = toJSON . runProperName

qualifiedToJSON :: (a -> Text) -> Qualified a -> Value
qualifiedToJSON f = toJSON . showQualified f

moduleNameToJSON :: ModuleName -> Value
moduleNameToJSON = toJSON . runModuleName

commentToJSON :: Comment -> Value
commentToJSON (LineComment  t) = toJSON ("Line",  t)
commentToJSON (BlockComment t) = toJSON ("Block", t)

(|>) :: a -> (a -> b) -> b
(|>) = flip ($)
infixl 0 |>

-}
