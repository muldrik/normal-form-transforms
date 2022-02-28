module Conversions where
import Formula
import Foreign (deRefStablePtr)


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


isNNF :: Formula -> Bool
isNNF (Not Tru) = True
isNNF (Not Fls) = True
isNNF (Not (Var _)) = True
isNNF (Not _) = False
isNNF (f1 :| f2) = isNNF f1 && isNNF f2
isNNF (f1 :& f2) = isNNF f1 && isNNF f2
isNNF _ = True


fromNNFtoDNF :: NNF -> DNF
fromNNFtoDNF (NNF f) = DNF (fromNNFtoDNF' f) where
  fromNNFtoDNF' (a :& (b :| c)) = fromNNFtoDNF' (a :& b) :| fromNNFtoDNF' (a :& c)
  fromNNFtoDNF' ((b :| c) :& a) = fromNNFtoDNF' (a :& b) :| fromNNFtoDNF' (a :& c)
  fromNNFtoDNF' (a :& b) | not (isConjunction a) = fromNNFtoDNF' $ fromNNFtoDNF' a :& b
                         | not (isConjunction b) = fromNNFtoDNF' $ a :& fromNNFtoDNF' b
                         | otherwise = a :& b
  fromNNFtoDNF' (a :| b) = fromNNFtoDNF' a :| fromNNFtoDNF' b
  fromNNFtoDNF' x = x


toDNF :: Formula -> DNF
toDNF = fromNNFtoDNF . toNNF


isLiteralOrConst :: Formula -> Bool
isLiteralOrConst Tru = True
isLiteralOrConst Fls = True
isLiteralOrConst (Var _) = True
isLiteralOrConst (Not (Var _)) = True
isLiteralOrConst _ = False


isConjunction :: Formula -> Bool
isConjunction (f1 :& f2) = isConjunction f1 && isConjunction f2
isConjunction (_ :| _) = False
isConjunction f = isLiteralOrConst f


isDisjunction :: Formula -> Bool
isDisjunction (f1 :| f2) = isDisjunction f1 || isDisjunction f2
isDisjunction (_ :& _) = False
isDisjunction f = isLiteralOrConst f


isDNF :: Formula -> Bool
isDNF conj@(_ :& _) = isConjunction conj
isDNF (f1 :| f2) = isDNF f1 && isDNF f2
isDNF f = isLiteralOrConst f


fromNNFtoCNF :: NNF -> CNF
fromNNFtoCNF (NNF f) = CNF (fromNNFtoCNF' f) where
  fromNNFtoCNF' (a :| (b :& c)) = fromNNFtoCNF' (a :| b) :& fromNNFtoCNF' (a :| c)
  fromNNFtoCNF' ((b :& c) :| a) = fromNNFtoCNF' (a :| b) :& fromNNFtoCNF' (a :| c)
  fromNNFtoCNF' (a :| b) | not (isDisjunction a) = fromNNFtoCNF' $ fromNNFtoCNF' a :| b
                         | not (isDisjunction b) = fromNNFtoCNF' $ a :| fromNNFtoCNF' b
                         | otherwise = a :| b
  fromNNFtoCNF' (a :& b) = fromNNFtoCNF' a :& fromNNFtoCNF' b
  fromNNFtoCNF' x = x


toCNF :: Formula -> CNF
toCNF = fromNNFtoCNF . toNNF


isCNF :: Formula -> Bool
isCNF disj@(_ :| _) = isDisjunction disj
isCNF (f1 :& f2) = isCNF f1 && isCNF f2
isCNF f = isLiteralOrConst f


chooseFreshVar :: String -> Symb
chooseFreshVar key = "newvar" ++ key


toEquisatCNF :: Formula -> CNF
toEquisatCNF f = let
  (NNF negationNormalized) = toNNF f
  (conjunctions, headSymb) =  helper "0" negationNormalized in
    CNF (getCNF conjunctions :& Var headSymb) where

  helper :: String -> Formula -> (CNF, Symb)
  helper depthKey (f1 :| f2) = binaryOpBranch depthKey f1 f2 (:|)
  helper depthKey (f1 :& f2) = binaryOpBranch depthKey f1 f2 (:&)
  helper depthKey (f1 :-> f2) = binaryOpBranch depthKey f1 f2 (:->)
  helper depthKey (f1 :<-> f2) = binaryOpBranch depthKey f1 f2 (:<->)
  helper depthKey (Not form) = unaryOpBranch depthKey form Not
  helper _ form@(Var x) = (CNF form , x)
  helper _ form = (CNF form, "")

  binaryOpBranch :: String -> Formula -> Formula -> (Formula -> Formula -> Formula) -> (CNF, Symb)
  binaryOpBranch depthKey f1 f2 op = let
    newvar = chooseFreshVar depthKey
    (leftFormula, leftVar) = helper ('0' : depthKey) f1
    (rightFormula, rightVar) = helper ('1' : depthKey) f2
    newConj = toCNF $ Var newvar :<-> (Var rightVar `op` Var leftVar) in
      (CNF (getCNF newConj :& getCNF leftFormula :& getCNF rightFormula), newvar)

  unaryOpBranch :: String -> Formula -> (Formula -> Formula) -> (CNF, Symb)
  unaryOpBranch depthKey form op = let
    newvar = chooseFreshVar depthKey
    (subFormula, subVar) = helper ('0' : depthKey) form
    newConj = toCNF $ Var newvar :<-> op (Var subVar) in
      (CNF (getCNF newConj :& getCNF subFormula), newvar)

