{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This module provides the 'ToExp' class, which can be used to convert values
-- into Template Haskell 'Exp' values that represent expressions that evaluate
-- to the original values. This makes it possible to persist/reify values
-- between compile-time and runtime.
module Language.Haskell.TH.ToExp
  ( ToExp(..)
  , toExpIntegral
  ) where

import GHC.Generics
import Language.Haskell.TH.Syntax as TH

import Data.Ratio (Ratio, (%), numerator, denominator)
import Data.Word (Word8)

--------------------------------------------------------------------------------
-- ToExp

-- | Converts arbitrary values to Template Haskell 'Exp' values, which can be
-- used in splices to reconstruct the original value. This is useful to persist
-- a compile-time value to a runtime one.
--
-- >>> toExp (Just 1)
-- AppE (ConE GHC.Base.Just) (LitE (IntegerL 1))
--
-- This class can be automatically derived for types with a 'Generic' instance.
class ToExp a where
  toExp :: a -> Exp

  default toExp :: (Generic a, GToExp (Rep a)) => a -> Exp
  toExp = gToExp . from

instance ToExp Integer where
  toExp n = SigE (LitE $ IntegerL n) (ConT ''Integer)

-- | An implementation of 'toExp' that can be used for types with an 'Integral'
-- instance.
--
-- >>> toExpIntegral (ConT ''Int) 3
-- SigE (LitE (IntegerL 3)) (ConT GHC.Types.Int)
toExpIntegral
  :: Integral a
  => Type -- ^ the Template Haskell 'Type' representing the type of @a@
  -> a    -- ^ the value to be converted to an 'Exp'
  -> Exp
toExpIntegral ty n = SigE (LitE $ IntegerL (toInteger n)) ty

instance ToExp Int where toExp = toExpIntegral (ConT ''Int)
instance ToExp Word8 where toExp = toExpIntegral (ConT ''Word8)

instance ToExp a => ToExp (Ratio a) where
  toExp r = AppE (AppE (VarE '(%)) (toExp $ numerator r)) (toExp $ denominator r)

instance ToExp Char where
  toExp = LitE . CharL

-- | produces expressions that use 'ListE' instead of 'AppE' and ':' to
-- make them prettier
instance ToExp a => ToExp [a] where
  toExp = ListE . map toExp

instance ToExp ()
instance ToExp Bool
instance ToExp Ordering
instance ToExp a => ToExp (Maybe a)
instance (ToExp a, ToExp b) => ToExp (Either a b)

instance (ToExp a, ToExp b) => ToExp (a, b)
instance (ToExp a, ToExp b, ToExp c) => ToExp (a, b, c)
instance (ToExp a, ToExp b, ToExp c, ToExp d) => ToExp (a, b, c, d)
instance (ToExp a, ToExp b, ToExp c, ToExp d, ToExp e) => ToExp (a, b, c, d, e)
instance (ToExp a, ToExp b, ToExp c, ToExp d, ToExp e, ToExp f) => ToExp (a, b, c, d, e, f)
instance (ToExp a, ToExp b, ToExp c, ToExp d, ToExp e, ToExp f, ToExp g) => ToExp (a, b, c, d, e, f, g)

-- instances for Template Haskell types
instance ToExp TH.AnnLookup
instance ToExp TH.AnnTarget
instance ToExp TH.Bang
instance ToExp TH.Body
instance ToExp TH.Callconv
instance ToExp TH.Clause
instance ToExp TH.Con
instance ToExp TH.Dec
instance ToExp TH.DecidedStrictness
instance ToExp TH.Exp
instance ToExp TH.Extension
instance ToExp TH.FamilyResultSig
instance ToExp TH.Fixity
instance ToExp TH.FixityDirection
instance ToExp TH.Foreign
instance ToExp TH.FunDep
instance ToExp TH.Guard
instance ToExp TH.Info
instance ToExp TH.InjectivityAnn
instance ToExp TH.Inline
instance ToExp TH.Lit
instance ToExp TH.Match
instance ToExp TH.ModName
instance ToExp TH.Module
instance ToExp TH.ModuleInfo
instance ToExp TH.Name
instance ToExp TH.NameFlavour
instance ToExp TH.NameSpace
instance ToExp TH.OccName
instance ToExp TH.Overlap
instance ToExp TH.Pat
instance ToExp TH.Phases
instance ToExp TH.PkgName
instance ToExp TH.Pragma
instance ToExp TH.Range
instance ToExp TH.Role
instance ToExp TH.RuleBndr
instance ToExp TH.RuleMatch
instance ToExp TH.Safety
instance ToExp TH.SourceStrictness
instance ToExp TH.SourceUnpackedness
instance ToExp TH.Stmt
instance ToExp TH.TyLit
instance ToExp TH.Type
instance ToExp TH.TypeFamilyHead
instance ToExp TH.TySynEqn
instance ToExp TH.TyVarBndr

--------------------------------------------------------------------------------
-- GToExp

-- | Top-level class for converting generic representations to expressions, used
-- as the default implementation for 'toExp'. There is only one instance, over
-- 'D1', but that’s because gToExp' handles most of the details.
--
-- Since there’s only one instance, making this a class is not strictly
-- necessary, but keeping it a class makes the default signature for 'toExp'
-- simpler, which is useful for the documentation.
class GToExp f where
  gToExp :: f a -> Exp

-- | The top-level instance for converting generic representations to
-- expressions. This instance extracts the module name and package name from
-- a datatype and threads it along so that Template Haskell 'Name's can be
-- reconstructed from the constructor names.
instance (Datatype d, GToExp' (D1 d cs)) => GToExp (D1 d cs) where
  gToExp x = gToExp' x (Module package name)
    where name = ModName $ moduleName x
          package = PkgName $ packageName x

-- | The class that handles type constructors. The 'C1' instance performs the
-- bulk of the work, combining constructor information and the 'Module'
-- information produced by 'GToExp'.
class GToExp' f where
  gToExp' :: f a -> Module -> Exp

instance GToExp' f => GToExp' (D1 t f) where
  gToExp' (M1 x) = gToExp' x

instance (GToExp' f, GToExp' g) => GToExp' (f :+: g) where
  gToExp' (L1 x) = gToExp' x
  gToExp' (R1 x) = gToExp' x

-- | The instance that actually does most of the conversion to an 'Exp'. It
-- first constructs a 'Name' from the generic information provided, combined
-- with the 'Module' produced from the datatype information in the 'GToExp'
-- instance for 'D1'.
--
-- The 'Name' is subsequently applied to the necessary arguments by deferring to
-- the final class, 'GProductToExps', which just applies 'toExp' to each member
-- of the product type and returns a list of 'Exp' values.
instance (Constructor c, GProductToExps f) => GToExp' (C1 c f) where
  gToExp' x (Module pkgName modName) = foldl AppE (ConE name) (gProductToExps x)
    where nameStr = conName x
          nameFlavour = NameG DataName pkgName modName
          name = Name (OccName nameStr) nameFlavour

-- | As explained by the documentation for 'GToExp'', this class produces
-- expressions for each argument of a constructor by traversing the product type
-- information available.
class GProductToExps f where
  gProductToExps :: f a -> [Exp]

-- | Nullary constructors are easy — they have no args, so we just produce an
-- empty list.
instance GProductToExps U1 where
  gProductToExps _ = []

-- | For actual values, defer to 'toExp' to do the whole process over again, or
-- use a manually-defined instance for things like literals.
instance ToExp c => GProductToExps (K1 i c) where
  gProductToExps (K1 x) = [toExp x]

instance GProductToExps f => GProductToExps (M1 i t f) where
  gProductToExps (M1 x) = gProductToExps x

-- | For a tree of product type elements, recur over each side of the tree and
-- append the results.
instance (GProductToExps a, GProductToExps b) => GProductToExps (a :*: b) where
  gProductToExps (x :*: y) = gProductToExps x ++ gProductToExps y
