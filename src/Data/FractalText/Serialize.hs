{-# LANGUAGE DeriveGeneric #-}

module Data.FractalText.Serialize where

import Data.FractalText.Annotate
import Data.FractalText.Type
import Data.FractalText.Util

import Control.Monad (forM_)
import Data.List (intersperse)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TLB
import qualified Data.Text.Short as ST
import qualified Data.Vector as V
import Generic.Data
import qualified Streaming as S
import qualified Streaming.Prelude as S
import System.IO (Newline(..), nativeNewline)

osNewline :: T.Text
osNewline = T.pack $ case nativeNewline of
  LF -> "\n"
  CRLF -> "\r\n"

nl :: TLB.Builder
nl = TLB.fromText osNewline

indent :: Int -> TLB.Builder
indent i = TLB.fromString $ replicate i ' '

renderSurplusLines :: V.Vector SurplusLine -> TLB.Builder
renderSurplusLines sls = mconcat . intersperse nl
  . V.toList $ flip V.map sls $ \sl -> case sl of
    SurplusLine'Blank -> mempty
    SurplusLine'Comment i ts -> indent i <> TLB.fromText ts

renderSurplusLinesA :: V.Vector SurplusLine -> TLB.Builder
renderSurplusLinesA v = if V.null v then mempty else renderSurplusLines v <> nl
renderSurplusLinesB :: V.Vector SurplusLine -> TLB.Builder
renderSurplusLinesB v = if V.null v then mempty else nl <> renderSurplusLines v

data SerializeError
  = SerializeError'InvalidDocument
  deriving (Generic, Bounded, Enum, Eq, Ord, Show)

serialize :: Document -> Either SerializeError TL.Text
serialize doc@(ia, sls0)
  | isValidDocument doc =
    let x = go ia
     in Right $ TLB.toLazyText $ x <> renderSurplusLinesA sls0 <> nl
  | otherwise = Left SerializeError'InvalidDocument
 where
  go ItemA'EmptyList = mempty
  go (ItemA'List i es) =
    let f (ListElemA sls q ts) =
          let x0 = TLB.fromText ts
              x1 = if q then quotationMark <> x0 <> quotationMark else x0
           in renderSurplusLinesB sls <> indent i <> x1
     in mconcat $ intersperse nl $ map f $ V.toList es
  go (ItemA'Dict i des) =
    let f (DictElemA sls k v) =
          renderSurplusLinesB sls <> indent i <> renderKey k <> case v of
            ItemA'EmptyList -> mempty
            _ -> nl <> go v
     in mconcat $ intersperse nl $ map f $ V.toList des

  renderKey k = TLB.fromString ":" <> TLB.fromText (ST.toText k)
  quotationMark = TLB.fromString "\""

serializeNaked :: Int -> Item -> Either SerializeError TL.Text
serializeNaked isucc = serialize . (\it -> (it, V.empty)) . annotate isucc

dumpLine :: Monad m => Document
         -> Either SerializeError (S.Stream (S.Of Line) m ())
dumpLine doc@(ia, sls0)
  | isValidDocument doc = Right $ go ia >> dumpSurplusLines sls0
  | otherwise = Left SerializeError'InvalidDocument
 where
  go ItemA'EmptyList = return ()
  go (ItemA'List i es) = forM_ es $ dumpListElem i
  go (ItemA'Dict i des) = forM_ des $ dumpDictElem i
  dumpSurplusLines = S.each . V.map f
   where
    f SurplusLine'Blank = Line'Blank
    f (SurplusLine'Comment i ts) = Line'Comment i ts
  dumpListElem i (ListElemA sls q ts) = do
    dumpSurplusLines sls
    S.yield $ Line'Value i
      $ if q then T.singleton '\"' <> ts <> T.singleton '\"' else ts
  dumpDictElem i (DictElemA sls k v) = do
    dumpSurplusLines sls
    S.yield $ Line'Key i k
    go v
