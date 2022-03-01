module Formula where
{-# LANGUAGE FlexibleInstances #-}

import Test.Tasty.QuickCheck as QC
import Control.Applicative

type Symb = String

infixl 3 :<->
infixl 4 :->
infixl 5 :|
infixl 6 :&

{-
data ExtendedBasisFormula = Classic Formula |
               ExtendedBasisFormula :-> ExtendedBasisFormula |
               ExtendedBasisFormula :<-> ExtendedBasisFormula
               deriving (Eq, Show)
-}


newtype StandardBasis = Stb {getStb :: Formula} deriving (Eq, Show)
newtype NNF = NNF {getNNF :: Formula} deriving (Eq, Show)
newtype DNF = DNF {getDNF :: Formula} deriving (Eq, Show)
newtype CNF = CNF {getCNF :: Formula} deriving (Eq, Show)
newtype SmallFormula = SmallFormula {getSmallFormula :: Formula} deriving (Eq, Show)



data Formula = Tru | Fls |
               Var Symb |
               Not Formula |
               Formula :| Formula |
               Formula :& Formula |
               Formula :-> Formula |
               Formula :<-> Formula
               deriving (Eq, Show)



instance Arbitrary Formula where
  arbitrary = let
    varNames = (:[]) <$> choose ('a', 'g')
    in frequency 
      [
        (10, return Tru),
        (10, return Fls),
        (50, Var <$> varNames),
        (10, Not <$> arbitrary),
        (10, liftA2 (:|) arbitrary arbitrary),
        (10, liftA2 (:&) arbitrary arbitrary),
        (10, liftA2 (:->) arbitrary arbitrary),
        (10, liftA2 (:<->) arbitrary arbitrary)
      ]


instance Arbitrary SmallFormula where
  arbitrary = SmallFormula <$> helper 3 where
    helper :: Int -> Gen Formula
    helper depth | depth <= 0 = frequency 
      [
        (4, Var <$> varNames),
        (1, return Tru),
        (1, return Fls)
      ] where 
          varNames = (:[]) <$> choose ('a', 'g')
    helper depth = frequency 
      [
        (5, return Tru),
        (5, return Fls),
        (5, Var <$> varNames),
        (10, Not <$> helper (depth-1)),
        (10, liftA2 (:|) (helper (depth-1)) (helper (depth-1))),
        (10, liftA2 (:&) (helper (depth-1)) (helper (depth-1))),
        (10, liftA2 (:->) (helper (depth-1)) (helper (depth-1))),
        (10, liftA2 (:<->) (helper (depth-1)) (helper (depth-1)))
      ] where
          varNames = (:[]) <$> choose ('a', 'g')
