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

--Unit testing properties
unitTests :: TestTree
unitTests = testGroup "Unit tests"
  [
    testGroup "Checking is a formula is in NNF"
      [
        testCase "Single var" $ isNNF (Var "x") @? "",
        testCase "Top" $ isNNF Tru @? "",
        testCase "Bot" $ isNNF Fls @? "",
        testCase "Small formula" $ isNNF (Var "x" :| (Not (Var "y") :& Var "z" :| (Not (Var "p") :& Tru))) @? "",
        testCase "Double Negation" $ (not . isNNF) (Not $ Not $ Var "x") @? "",
        testCase "Implication" $ (not . isNNF) (Var "x" :-> Var "y") @? "",
        testCase "Equivalence" $ (not . isNNF) (Var "x" :<-> Var "y") @? "",
        testCase "Not bottom-level negation" $ (not . isNNF) (Var "x" :| Not (Var "y" :& Var "z")) @? ""
      ],
    testGroup "Checking if a formula is in DNF"
      [
        testCase "Single conjunction" $ isDNF (Var "x" :& Tru :& Fls :& Not (Var "y")) @? "",
        testCase "Multiple conjunctions" $ isDNF (Var "x" :| Var "y" :& Not (Var "z") :| Fls :| Tru) @? "",
        testCase "Invalid DNF" $ (not . isDNF) (Var "x" :& (Var "y" :| Var "z")) @? ""
      ],
    testGroup "Checking if a formula is in CNF"
      [
        testCase "Single disjunction" $ isCNF (Var "x" :| Tru :| Fls :| Not (Var "y")) @? "",
        testCase "Multiple disjunctions" $ isCNF (Var "x" :& (Var "y" :| Not (Var "z")) :& (Tru :| Fls)) @? "",
        testCase "Invalid CNF" $ (not . isCNF) (Var "x" :| (Var "y" :& Tru)) @? ""
      ]
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
    QC.testProperty "Tseytin transformation produces a CNF" $ isCNF . getCNF . toEquisatCNF,
    QC.testProperty "A CNF is either a big conjunction, big disjunction or not a DNF" $ \f -> isCNF f QC.==> isConjunction f || isDisjunction f || not (isDNF f),
    QC.testProperty "A DNF is either a big disjunction, big conjunction or not a CNF" $ \f -> isDNF f QC.==> isDisjunction f || isConjunction f || not (isCNF f)
  ]

