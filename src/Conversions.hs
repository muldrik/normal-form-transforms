module Conversions where
import Formula


-- Transform the formula to only use negation, OR, AND and constants
toStandardBasis :: Formula -> StandardBasis
toStandardBasis = Stb . fromExtendedBasis where
  fromExtendedBasis :: Formula -> Formula
  fromExtendedBasis (f1 :| f2) = fromExtendedBasis f1 :| fromExtendedBasis f2
  fromExtendedBasis (f1 :& f2) = fromExtendedBasis f1 :& fromExtendedBasis f2
  fromExtendedBasis (f1 :-> f2) = Not (fromExtendedBasis f1) :| fromExtendedBasis f2
  fromExtendedBasis (f1 :<-> f2) = let
    f1' = fromExtendedBasis f1
    f2' = fromExtendedBasis f2 in
      f1' :& f2' :| Not f1' :& Not f2'
  fromExtendedBasis (Not f) = Not (fromExtendedBasis f)
  fromExtendedBasis f = f


-- Convert the formula to negation normal form
toNNF :: Formula  -> NNF
toNNF f = let (Stb inStandardBasis) = toStandardBasis f in NNF (toNNF' inStandardBasis) where
  toNNF' :: Formula -> Formula
  toNNF' (f1 :| f2) = toNNF' f1 :| toNNF' f2
  toNNF' (f1 :& f2) = toNNF' f1 :& toNNF' f2

  toNNF' (Not (Not x)) = toNNF' x
  toNNF' (Not (f1 :| f2)) = let
    f1' = toNNF' (Not f1)
    f2' = toNNF' (Not f2) in
      f1' :& f2'
  toNNF' (Not (f1 :& f2)) = let
    f1' = toNNF' (Not f1)
    f2' = toNNF' (Not f2) in
      f1' :| f2'
  toNNF' (Not Tru) = Fls
  toNNF' (Not Fls) = Tru
  toNNF' g@(Not (Var _)) = g
  toNNF' Tru = Tru
  toNNF' Fls = Fls
  toNNF' (Var x) = Var x
  toNNF' (_ :-> _) = error "Undexped -> operation. Standard basis formula expected"
  toNNF' (Not (_ :-> _)) = error "Undexped -> operation. Standard basis formula expected"
  toNNF' (_ :<-> _) = error "Undexped <-> operation. Standard basis formula expected"
  toNNF' (Not (_ :<-> _)) = error "Undexped <-> operation. Standard basis formula expected"


-- Check if a formula is in negation normal form. Note that negating constants is allowed
isNNF :: Formula -> Bool
isNNF (Not Tru) = True
isNNF (Not Fls) = True
isNNF (Not (Var _)) = True
isNNF (Not _) = False
isNNF (f1 :| f2) = isNNF f1 && isNNF f2
isNNF (f1 :& f2) = isNNF f1 && isNNF f2
isNNF (_ :-> _) = False
isNNF (_ :<-> _) = False
isNNF _ = True


-- Convert a formula from NNF to DNF using the distributive property of AND. 
-- Note that if the formula is actually not in NNF, an incorrect result is generated and no error is thrown.
fromNNFtoDNF :: NNF -> DNF
fromNNFtoDNF (NNF f) = DNF (fromNNFtoDNF' f) where
  fromNNFtoDNF' (a :& (b :| c)) = fromNNFtoDNF' (a :& b) :| fromNNFtoDNF' (a :& c)
  fromNNFtoDNF' ((b :| c) :& a) = fromNNFtoDNF' (a :& b) :| fromNNFtoDNF' (a :& c)
  fromNNFtoDNF' (a :& b) | not (isConjunction a) = fromNNFtoDNF' $ fromNNFtoDNF' a :& b
                         | not (isConjunction b) = fromNNFtoDNF' $ a :& fromNNFtoDNF' b
                         | otherwise = a :& b
  fromNNFtoDNF' (a :| b) = fromNNFtoDNF' a :| fromNNFtoDNF' b
  fromNNFtoDNF' x = x


-- Convert an arbitrary formula to DNF
toDNF :: Formula -> DNF
toDNF = fromNNFtoDNF . toNNF


-- Check if the formula is a monomial
isLiteralOrConst :: Formula -> Bool
isLiteralOrConst Tru = True
isLiteralOrConst Fls = True
isLiteralOrConst (Var _) = True
isLiteralOrConst (Not (Var _)) = True
isLiteralOrConst (Not Tru) = True
isLiteralOrConst (Not Fls) = True
isLiteralOrConst _ = False


-- Check if a formula is a valid conjunction to be used in DNF
isConjunction :: Formula -> Bool
isConjunction (f1 :& f2) = isConjunction f1 && isConjunction f2
isConjunction (_ :| _) = False
isConjunction f = isLiteralOrConst f


-- Check if a formula is a valid disjunction to be used in CNF
isDisjunction :: Formula -> Bool
isDisjunction (f1 :| f2) = isDisjunction f1 && isDisjunction f2
isDisjunction (_ :& _) = False
isDisjunction f = isLiteralOrConst f


-- Check if an arbitrary formula is in DNF
isDNF :: Formula -> Bool
isDNF conj@(_ :& _) = isConjunction conj
isDNF (f1 :| f2) = isDNF f1 && isDNF f2
isDNF f = isLiteralOrConst f


-- Convert a formula from NNF to CNF using the distributive property of OR
fromNNFtoCNF :: NNF -> CNF
fromNNFtoCNF (NNF f) = CNF (fromNNFtoCNF' f) where
  fromNNFtoCNF' (a :| (b :& c)) = fromNNFtoCNF' (a :| b) :& fromNNFtoCNF' (a :| c)
  fromNNFtoCNF' ((b :& c) :| a) = fromNNFtoCNF' (a :| b) :& fromNNFtoCNF' (a :| c)
  fromNNFtoCNF' (a :| b) | not (isDisjunction a) = fromNNFtoCNF' $ fromNNFtoCNF' a :| b
                         | not (isDisjunction b) = fromNNFtoCNF' $ a :| fromNNFtoCNF' b
                         | otherwise = a :| b
  fromNNFtoCNF' (a :& b) = fromNNFtoCNF' a :& fromNNFtoCNF' b
  fromNNFtoCNF' x = x


-- Arbitrary formula to CNF
toCNF :: Formula -> CNF
toCNF = fromNNFtoCNF . toNNF


-- Check if an arbitrary formula is in CNF
isCNF :: Formula -> Bool
isCNF disj@(_ :| _) = isDisjunction disj
isCNF (f1 :& f2) = isCNF f1 && isCNF f2
isCNF f = isLiteralOrConst f


-- Simple variable name generator. Note that no collision testing in performed and other variables named newvar<N> should not be used
chooseFreshVar :: String -> Symb
chooseFreshVar key = "newvar" ++ key


-- Convert an arbitrary formula to equvisatisfiable formula in CNF using Tseytin transformation
toEquisatCNF :: Formula -> CNF
toEquisatCNF f = let -- Special case for the root node, must be added to the resulting formula
  (conjunctions, headSymb) =  helper "0" f in
    CNF (getCNF conjunctions :& Var headSymb) where

  -- Given a formula generate root symbol equivalent to the subtree and all CNF restrictions in the subtree
  -- depthKey is used to generate unique names in both subtrees
  helper :: String -> Formula -> (CNF, Symb)
  helper depthKey (f1 :| f2) = binaryOpBranch depthKey f1 f2 (:|)
  helper depthKey (f1 :& f2) = binaryOpBranch depthKey f1 f2 (:&)
  helper depthKey (f1 :-> f2) = binaryOpBranch depthKey f1 f2 (:->)
  helper depthKey (f1 :<-> f2) = binaryOpBranch depthKey f1 f2 (:<->)
  helper depthKey (Not form) = unaryOpBranch depthKey form Not
  helper _ (Var x) = (CNF Tru , x)
  helper depth constant = let 
    newvar = chooseFreshVar depth
    newConj = toCNF $ Var newvar :<-> constant in
    (newConj, newvar)

  -- Avoid copy-paste for binary operations
  binaryOpBranch :: String -> Formula -> Formula -> (Formula -> Formula -> Formula) -> (CNF, Symb)
  binaryOpBranch depthKey f1 f2 op = let
    newvar = chooseFreshVar depthKey
    (leftFormula, leftVar) = helper ('0' : depthKey) f1
    (rightFormula, rightVar) = helper ('1' : depthKey) f2
    newConj = toCNF $ Var newvar :<-> (Var leftVar `op` Var rightVar) in
      (CNF (getCNF newConj :& getCNF leftFormula :& getCNF rightFormula), newvar)

  -- Avoid (possible) copy-paste for unary opeerations
  unaryOpBranch :: String -> Formula -> (Formula -> Formula) -> (CNF, Symb)
  unaryOpBranch depthKey form op = let
    newvar = chooseFreshVar depthKey
    (subFormula, subVar) = helper ('0' : depthKey) form
    newConj = toCNF $ Var newvar :<-> op (Var subVar) in
      (CNF (getCNF newConj :& getCNF subFormula), newvar)


