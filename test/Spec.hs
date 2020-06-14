import           Test.Hspec
import           Test.Hspec.Core.QuickCheck     ( modifyMaxSize )
import           Test.QuickCheck
import           Control.Exception              ( evaluate )
import           BloomCheck
import qualified Easy                          as B

main :: IO ()
main = hspec $ do
  describe "BloomFilter" $ do
    context "when used with ints" $ do
      modifyMaxSize (const 1000) $ it "reports a positive membership test after adding an element" $ property $
        \elt -> prop_one_present (undefined :: Int) (elt :: Int)
      modifyMaxSize (const 1000) $ it "reports a positive membership test for each item after adding many elements" $ property $
        \elt -> prop_all_present (undefined :: Int) (elt :: [Int])
      modifyMaxSize (const 1000) $ it "suggests correct sizes" $ property $
        prop_suggestions_sane

    context "when used with doubles" $ do
      modifyMaxSize (const 1000) $ it "reports a positive membership test after adding an element" $ property $
        \elt -> prop_one_present (undefined :: Double) (elt :: Double)
      modifyMaxSize (const 1000) $ it "reports a positive membership test for each item after adding many elements" $ property $
        \elt -> prop_all_present (undefined :: Double) (elt :: [Double])
      modifyMaxSize (const 1000) $ it "suggests correct sizes" $ property $
        prop_suggestions_sane

    context "when used with String" $ do
      modifyMaxSize (const 1000) $ it "reports a positive membership test after adding an element" $ property $
        \elt -> prop_one_present (undefined :: String) (elt :: String)
      modifyMaxSize (const 1000) $ it "reports a positive membership test for each item after adding many elements" $ property $
        \elt -> prop_all_present (undefined :: String) (elt :: [String])
      modifyMaxSize (const 1000) $ it "suggests correct sizes" $ property $
        prop_suggestions_sane
