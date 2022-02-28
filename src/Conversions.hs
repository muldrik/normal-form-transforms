module Conversions where
import Formula


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
  toNNF' f@(Not (Var _)) = f
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
  fromNNFtoDNF' (a :& b) | not (isConjuction a) = fromNNFtoDNF' $ fromNNFtoDNF' a :& b
                         | not (isConjuction b) = fromNNFtoDNF' $ a :& fromNNFtoDNF' b
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


isConjuction :: Formula -> Bool
isConjuction (f1 :& f2) = isConjuction f1 && isConjuction f2
isConjuction (_ :| _) = False
isConjuction f = isLiteralOrConst f



isDNF conj@(_ :& _) = isConjuction conj
isDNF (f1 :| f2) = isDNF f1 && isDNF f2
isDNF f = isLiteralOrConst f





