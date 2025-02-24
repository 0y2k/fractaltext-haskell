module Json where

import Data.FractalText
import Data.FractalText.Annotate
import Data.FractalText.Type

import Control.Monad (forM)
import Data.Maybe (catMaybes)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TLIO
import qualified Data.Text.Short as ST
import qualified Data.Vector as V
import System.Directory (doesFileExist, listDirectory)
import System.FilePath ((</>))
import System.IO (IOMode(..), hSetEncoding, hSetNewlineMode, nativeNewlineMode,
                  noNewlineTranslation, openFile, utf8)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import qualified Text.JSON as J

data TestLoad
  = TestLoad'Nothing
  | TestLoad'Success FilePath FilePath
  | TestLoad'Failure FilePath
  deriving (Eq, Show)

newtype I = I { unI :: Item }
newtype IA = IA { unIA :: ItemA }

instance J.JSON I where
  readJSON (J.JSArray arr) =
    I . Item'List . V.fromList . map ListElem <$> forM arr J.readJSON
  readJSON (J.JSObject jsobj) =
    fmap (I . Item'Dict . V.fromList)
    $ forM (J.fromJSObject jsobj) $ \(k, v) -> do
      it <- J.readJSON v
      return $ DictElem (ST.pack k) (unI it)
  readJSON _ = J.Error "cannot convert json to fractaltext item"
  showJSON _ = error "unimplemented"
instance J.JSON IA where
  readJSON jsv = IA . annotate 4 . unI <$> J.readJSON jsv
  showJSON _ = error "unimplemented"

loadPlaintext :: FilePath -> IO TL.Text
loadPlaintext file = do
  h <- openFile file ReadMode
  hSetEncoding h utf8
  hSetNewlineMode h noNewlineTranslation
  TLIO.hGetContents h

loadJSON :: FilePath -> IO (Maybe Item)
loadJSON file = do
  h <- openFile file ReadMode
  hSetEncoding h utf8
  hSetNewlineMode h nativeNewlineMode
  ts <- TLIO.hGetContents h
  return $ case J.decode $ TL.unpack ts of
    J.Ok (I i) -> Just i
    J.Error _err -> Nothing

test_json :: IO [TestTree]
test_json = do
  let dir = "vendor"
        </> "github.com"
        </> "0y2k"
        </> "fractaltext-test"
        </> "load"
      skipLoad = []
  ds <- listDirectory dir
  ts <- forM ds $ \d0 -> do
    let d = dir </> d0
        fileLoadIn = d </> "load_in.ft"
        fileLoadOut = d </> "load_out.json"
        fileLoadErr = d </> "load_err.json"
    fli <- doesFileExist fileLoadIn
    tl <- if not fli then return TestLoad'Nothing else do
      flo <- doesFileExist fileLoadOut
      fle <- doesFileExist fileLoadErr
      return $ case (flo, fle) of
        (True, False) -> TestLoad'Success fileLoadIn fileLoadOut
        (False, True) -> TestLoad'Failure fileLoadIn
        (True, True) -> error $ d0 ++ ": ambiguous expected result"
        (False, False) -> error $ d0 ++ ": no expected result"
    return (d0, tl)
  tgl <- fmap (testGroup "load" . catMaybes) $ forM ts $ \(d, tl) ->
    if d `elem` skipLoad then return Nothing else case tl of
      TestLoad'Nothing -> return Nothing
      TestLoad'Success fin fout -> do
        mjout <- loadJSON fout
        case mjout of
          Just iout -> do
            pin <- loadPlaintext fin
            return $ Just $ testCase d $ case parseNaked pin of
              Right iin -> iin @?= iout
              Left err -> assertFailure $ show err
          Nothing -> return Nothing
      TestLoad'Failure fin -> do
        pin <- loadPlaintext fin
        return $ Just $ testCase d $ case parseNaked pin of
          Left _err -> return ()
          Right iin -> assertFailure
            $ "load succeed! it should fail: " ++ show iin
  return [tgl]
