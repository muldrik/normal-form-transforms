{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
module Equivalence where
import qualified Data.Set as St
import qualified Data.Map as Mp
import Data.Maybe (isJust)
import Formula

class Evaluatable a where
  evaluate :: a -> Mp.Map Symb Bool -> Maybe Bool -- Returns Just the evaluation if all variables are present in the lookup table, else Nothing
  freeVars :: a -> St.Set Symb -- Returns all freee variables present in a formula


instance Evaluatable StandardBasis  where
  evaluate (Stb f) = evaluate f
  freeVars (Stb f) = freeVars f

instance Evaluatable NNF where
  evaluate (NNF f) = evaluate f
  freeVars (NNF f) = freeVars f

instance Evaluatable DNF where
  evaluate (DNF f) = evaluate f
  freeVars (DNF f) = freeVars f

instance Evaluatable CNF where
  evaluate (CNF f) = evaluate f
  freeVars (CNF f) = freeVars f

instance Evaluatable SmallFormula where
  evaluate (SmallFormula f) = evaluate f
  freeVars (SmallFormula f) = freeVars f


instance Evaluatable Formula where 
  evaluate Tru _ = Just True
  evaluate Fls _ = Just False
  evaluate (Var x) m = x `Mp.lookup` m
  evaluate (Not f) m = do
    res <- evaluate f m
    return $ not res
  evaluate (f1 :| f2) m = do
    res1 <- evaluate f1 m
    res2 <- evaluate f2 m
    return $ res1 || res2
  evaluate (f1 :& f2) m = do
    res1 <- evaluate f1 m
    res2 <- evaluate f2 m
    return $ res1 && res2
  evaluate (f1 :-> f2) m = do
    res1 <- evaluate f1 m
    res2 <- evaluate f2 m
    return (not res1 || res2)
  evaluate (f1 :<-> f2) m = do
    res1 <- evaluate f1 m
    res2 <- evaluate f2 m
    return (res1 == res2)

  freeVars (Var x) = St.singleton x
  freeVars (t1 :& t2) = freeVars t1 `St.union` freeVars t2
  freeVars (t1 :| t2) = freeVars t1 `St.union` freeVars t2
  freeVars (Not f) = freeVars f
  freeVars Tru = St.empty
  freeVars Fls = St.empty
  freeVars (f1 :-> f2) = freeVars f1 `St.union` freeVars f2
  freeVars (f1 :<-> f2) = freeVars f1 `St.union` freeVars f2



generateAllBitmasks :: St.Set Symb -> [Mp.Map Symb Bool]
generateAllBitmasks s | St.null s = []
                      | otherwise =  Mp.fromList <$> foldr helper [[]] s where
  helper :: Symb -> [[(Symb, Bool)]] -> [[(Symb, Bool)]]
  helper symb res = (((symb, False) :) <$> res) ++ (((symb, True) :) <$> res)

equiv :: (Evaluatable a, Evaluatable b) => a -> b -> Bool
equiv f1 f2 = let
  vars1 = freeVars f1
  vars2 = freeVars f2
  bitmasks = generateAllBitmasks vars1 in
    vars1 == vars2 && all sameResult bitmasks where
      sameResult mask = let
        eval1 = evaluate f1 mask
        eval2 = evaluate f2 mask in
          eval1 == eval2 && isJust eval1