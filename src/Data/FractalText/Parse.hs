{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}

module Data.FractalText.Parse where

import Data.FractalText.Type
import Data.FractalText.Util

import Control.Monad (unless)
import Control.Monad.Trans.Class (MonadTrans(..))
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Short as ST
import qualified Data.Vector as V
import Generic.Data
import Prelude hiding (readList)
import qualified Streaming as S
import Streaming.Parse as SP
import qualified Streaming.Prelude as S

data ValidLine
  = ValidLine'Key (V.Vector SurplusLine) Int Key
  | ValidLine'Value (V.Vector SurplusLine) Int T.Text
  deriving (Generic, Eq, Show)

data ParseError
  = ParseError'InvalidIndent
  | ParseError'UnexpectedEndOfInput
  | ParseError'UnexpectedLine
  deriving (Generic, Bounded, Enum, Eq, Ord, Show)

toLine :: T.Text -> Either ParseError Line
toLine ts =
  let ts0 = T.unpack ts
      countPrefixSpaces = go 0
       where
        go !n (' ':xs) = go (succ n) xs
        go !n _ = n
      indentLevel = countPrefixSpaces ts0
   in case drop indentLevel ts0 of
        [] -> Right Line'Blank
        '#':ts1 -> Right $ Line'Comment indentLevel $ T.pack ts1
        ':':ts1 -> Right $ Line'Key indentLevel $ ST.pack ts1
        '\t':_ts1 -> Left ParseError'InvalidIndent
        ts1 -> Right $ Line'Value indentLevel $ T.pack ts1

loadLine :: TL.Text -> S.Stream (S.Of Line) (Either ParseError) ()
loadLine = S.mapM toLine . SP.folds (<>) T.empty id . splitLines . fromLazyText

toValidLine :: Monad m => S.Stream (S.Of Line) m ()
            -> S.Stream (S.Of ValidLine) m (V.Vector SurplusLine)
toValidLine s = go s []
 where
  go s0 ys = do
    ml <- lift $ S.next s0
    case ml of
      Left () -> return $ V.fromList $ reverse ys
      Right (Line'Blank, s1) -> go s1 $ SurplusLine'Blank : ys
      Right (Line'Comment i ts, s1) -> go s1 $ SurplusLine'Comment i ts : ys
      Right (Line'Key i k, s1) -> do
        S.yield $ ValidLine'Key (V.fromList $ reverse ys) i k
        go s1 []
      Right (Line'Value i ts, s1) -> do
        S.yield $ ValidLine'Value (V.fromList $ reverse ys) i ts
        go s1 []

validLine'Indent :: ValidLine -> Int
validLine'Indent (ValidLine'Key _sls i _k) = i
validLine'Indent (ValidLine'Value _sls i _ts) = i

parse :: TL.Text -> Either ParseError Document
parse ts0 = runParser (readItem [EQ] 0) $ toValidLine $ loadLine ts0
 where
  runParser p s = do
    (ia, l) <- State.runStateT p s
    ex <- S.next l
    case ex of
      Left vs -> return (ia, vs)
      Right _ -> Left ParseError'UnexpectedLine
  readItem cs i = do
    ml <- SP.peek
    case ml of
      Nothing -> return ItemA'EmptyList
      Just (ValidLine'Key _sls j _k) -> do
        unless ((i `compare` j) `elem` cs)
          $ lift $ Left ParseError'InvalidIndent
        des <- readDict j
        return $ ItemA'Dict j des
      Just (ValidLine'Value _sls j _ts) -> do
        unless ((i `compare` j) `elem` cs)
          $ lift $ Left ParseError'InvalidIndent
        es <- readList j
        return $ ItemA'List j es
  readDict i = postprocess <$> go []
   where
    go ys = do
      ml <- SP.draw
      case ml of
        Nothing -> return ys
        Just l@(ValidLine'Key sls j k) -> case i `compare` j of
          EQ -> do
            ml2 <- SP.peek
            case compare j . validLine'Indent <$> ml2 of
              Just LT -> do
                y <- readItem [LT] j
                go $ DictElemA sls k y : ys
              Just EQ -> go $ DictElemA sls k ItemA'EmptyList : ys
              _ -> return $ DictElemA sls k ItemA'EmptyList : ys
          GT -> do
            SP.unDraw l
            return ys
          LT -> lift $ Left ParseError'InvalidIndent
        Just l -> case i `compare` validLine'Indent l of
          GT -> do
            SP.unDraw l
            return ys
          _ -> lift $ Left ParseError'UnexpectedLine
    postprocess = V.fromList . reverse
  readList i = postprocess <$> go []
   where
    go ys = do
      ml <- SP.draw
      case ml of
        Nothing -> return ys
        Just l@(ValidLine'Value sls j ts) -> case i `compare` j of
          EQ ->
            let (pss, q) = processString ts
             in go $ ListElemA sls q pss : ys
          GT -> do
            SP.unDraw l
            return ys
          LT -> lift $ Left ParseError'InvalidIndent
        Just l -> case i `compare` validLine'Indent l of
          GT -> do
            SP.unDraw l
            return ys
          _ -> lift $ Left ParseError'UnexpectedLine
    postprocess = V.fromList . reverse
  processString ts = case T.unpack ts of
    '\"':ts1 -> case reverse ts1 of
      '\"':ts2 -> (T.pack $ reverse ts2, True)
      _ -> (ts, False)
    _ -> (ts, False)

parseNaked :: TL.Text -> Either ParseError Item
parseNaked ts = removeAnnotation . fst <$> parse ts
