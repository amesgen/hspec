{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Hspec.Core.Spec.Vault (
  Vault
, setValue
, getValue
) where

import           Prelude ()
import           Test.Hspec.Core.Compat

import           Data.Typeable
import           Data.Dynamic

newtype Vault = Vault [(TypeRep, Dynamic)]
  deriving (
#if MIN_VERSION_base(4,11,0)
    Semigroup,
#endif
    Monoid)

setValue :: Typeable v => v -> Vault -> Vault
setValue v (Vault vs) = Vault $ (typeOf v, toDyn v) : vs

getValue :: forall v. Typeable v => Vault -> Maybe v
getValue (Vault vs) = lookup (typeOf (undefined :: v)) vs >>= fromDynamic
