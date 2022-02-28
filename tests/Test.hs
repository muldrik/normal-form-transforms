import Test.Tasty.HUnit
import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Equivalence
import Formula
import Conversions
import System.Environment (setEnv)


main :: IO ()
main = do
    setEnv "TASTY_COLOR" "always"
    setEnv "TASTY_TIMEOUT" "40"
    defaultMain tests


tests :: TestTree
tests = testGroup "Tests" [unitTests, quickTests]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
  [
  ]

quickTests :: TestTree
quickTests = testGroup "Quick tests"
  [
    QC.testProperty "Conversion to standard basis preserves equivalence" $ \f -> toStandardBasis f `equiv` f,
    QC.testProperty "NNF conversion preserves equivalence" $ \f -> toNNF f `equiv` f,
    QC.testProperty "NNF conversion works" $ isNNF . getNNF . toNNF,
    QC.testProperty "DNF conversion preserves equivalence" $ \f -> toDNF (getSmallFormula f) `equiv` f,
    QC.testProperty "DNF conversion works" $ isDNF . getDNF . toDNF . getSmallFormula,
    QC.testProperty "CNF conversion preserves equivalence" $ \f -> toCNF (getSmallFormula f) `equiv` f,
    QC.testProperty "CNF conversion works" $ isCNF . getCNF . toCNF . getSmallFormula,
    QC.testProperty "Tseytin transformation produces equisatisfiable formula" $ \f -> f `equisat` toEquisatCNF (getSmallFormula f),
    QC.testProperty "Tseytin transformation produces a CNF" $ isCNF . getCNF . toEquisatCNF . getSmallFormula
  ]

