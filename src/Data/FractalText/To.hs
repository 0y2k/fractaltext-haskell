{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.FractalText.To
  ( ToItem(..)
  , ToKey(..)
  , ToItemError'Map(..)
  ) where

import Data.FractalText.Type

import Control.Monad (forM)
import Data.Bifunctor (Bifunctor(first))
import Data.Kind (Type)
import qualified Data.Map as M
import qualified Data.Text as TS
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Short as ST
import qualified Data.Vector as V
import Data.Void (Void)
import Generic.Data

class ToItem a where
  type ToItemError a :: Type
  toItem :: a -> Either (ToItemError a) Item
class ToKey a where
  type ToKeyError a :: Type
  toKey :: a -> Either (ToKeyError a) Key

instance ToItem Item where
  type ToItemError Item = Void
  toItem = Right

instance ToItem TS.Text where
  type ToItemError TS.Text = Void
  toItem = Right . Item'List . V.singleton . ListElem
instance ToItem TL.Text where
  type ToItemError TL.Text = Void
  toItem = Right . Item'List . V.singleton . ListElem . TL.toStrict

instance a ~ TS.Text => ToItem [a] where
  type ToItemError [a] = Void
  toItem = Right . Item'List . V.fromList . map ListElem
instance a ~ TS.Text => ToItem (V.Vector a) where
  type ToItemError (V.Vector a) = Void
  toItem = Right . Item'List . V.map ListElem

instance ToKey TS.Text where
  type ToKeyError TS.Text = Void
  toKey = Right . ST.fromText
instance ToKey TL.Text where
  type ToKeyError TL.Text = Void
  toKey = Right . ST.fromText . TL.toStrict
instance ToKey ST.ShortText where
  type ToKeyError ST.ShortText = Void
  toKey = Right

data ToItemError'Map k v
  = ToItemError'Map'KeyError (ToKeyError k)
  | ToItemError'Map'ValueError (ToItemError v)
  deriving (Generic)
instance (Eq (ToKeyError k), Eq (ToItemError v))
  => Eq (ToItemError'Map k v) where
    (==) = geq
instance (Show (ToKeyError k), Show (ToItemError v))
  => Show (ToItemError'Map k v) where
    showsPrec = gshowsPrec

instance (ToKey k, ToItem v) => ToItem (M.Map k v) where
  type ToItemError (M.Map k v) = ToItemError'Map k v
  toItem dic = fmap (Item'Dict . V.fromList)
    $ forM (M.toAscList dic) $ \(ks, vs) -> do
      kd <- first ToItemError'Map'KeyError $ toKey ks
      vd <- first ToItemError'Map'ValueError $ toItem vs
      return $ DictElem kd vd
