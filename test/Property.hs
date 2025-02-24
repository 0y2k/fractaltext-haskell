module Property where

import Data.FractalText
import Data.FractalText.Annotate
import Data.FractalText.Type

import qualified Data.Char as C
import Data.Either (isRight)
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Short as ST
import qualified Data.Vector as V
import qualified Hedgehog as H
import qualified Hedgehog.Gen as HG
import qualified Hedgehog.Range as HR
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

genItem' :: H.MonadGen m => H.Range Int -> H.Range Int -> H.Range Int -> m Item
genItem' rs rk rn = go
 where
  go = HG.recursive HG.choice
    [ return Item'EmptyList
    ]
    [ fmap (Item'List . V.fromList) $ HG.list rn
      $ ListElem . T.pack <$> HG.string rs char
    , Item'Dict . V.fromList <$> entries
    ]
  entries = do
    ks <- HG.set rn $ ST.pack <$> HG.string rk char
    vs <- HG.list rn go
    return $ zipWith DictElem (S.toAscList ks) vs
  char = HG.filterT C.isPrint HG.unicode

genItem :: H.MonadGen m => m Item
genItem = genItem' (HR.linear 0 64) (HR.linear 0 16) (HR.linear 1 32)

test_property :: TestTree
test_property = testGroup "property"
  [ testProperty "removeAnnotation . annotate === id" $ H.property $ do
    n <- H.forAll $ HG.integral $ HR.linear 1 8
    item <- H.forAll genItem
    removeAnnotation (annotate n item) H.=== item
  , testProperty "serialize . annotate === Right" $ H.property $ do
    n <- H.forAll $ HG.integral $ HR.linear 1 8
    item <- H.forAll genItem
    H.assert $ isRight $ serialize (annotate n item, V.empty)
  , testProperty "parseNaked <=< serializeNaked === Right" $ H.property $ do
    n <- H.forAll $ HG.integral $ HR.linear 1 8
    item <- H.forAll genItem
    let ets = serializeNaked n item
    case ets of
      Left err -> H.annotateShow err >> H.failure
      Right ts -> do
        let eit = parseNaked ts
        case eit of
          Left err -> H.annotateShow err >> H.failure
          Right it -> it H.=== item
  ]
