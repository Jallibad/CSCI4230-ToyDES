{-# LANGUAGE KindSignatures, ConstraintKinds, RankNTypes, FlexibleContexts, TypeOperators, TypeFamilies, FlexibleInstances, UndecidableInstances #-}

module BitVector where

import Control.Arrow ((***))
import Data.Ratio
import Data.Char (intToDigit)
import Data.Ord (comparing)
import Data.Bits
import Data.Vector.Fixed as Vec
import Data.Vector.Fixed.Boxed
import Data.Vector.Fixed.Cont
import Data.Function (on)
import GHC.TypeLits
import Data.Vector.Fixed.Cont
import Numeric (showIntAtBase)

--data BitVector (n :: Nat) = BitVector (Vec n Bool)
newtype BitVector (n :: Nat) = BitVector (Vec n Bool)

type Thing a = (Peano (a + 1) ~ S (Peano a), KnownNat a, ArityPeano (Peano a))

unwrap :: BitVector n -> Vec n Bool
unwrap (BitVector v) = v

vectorToInt :: (Integral a, Vector v Bool) => v Bool -> a
vectorToInt = Vec.foldl (\n d -> 2*n + if d then 1 else 0) 0

instance Thing a => Eq (BitVector a) where
	(==) = (==) `on` unwrap

instance Thing a => Show (BitVector a) where
	show = ("0b"++) . Vec.foldr (\b s -> (if b then '1' else '0') : s) "" . unwrap

instance Thing a => Num (BitVector a) where
	(+) = (fromInteger .) . on (+) toInteger
	(*) = (fromInteger .) . on (*) toInteger
	(-) = (fromInteger .) . on (-) toInteger
	abs = fromInteger . abs . toInteger
	signum _ = 1
	negate = error "Negation is not defined for boolean vectors"
	fromInteger = BitVector . Vec.reverse . Vec.generate . testBit

instance (Thing a, Eq (BitVector a)) => Bits (BitVector a) where
	xor = (BitVector .) . (Vec.zipWith xor `on` unwrap)
	(.&.) = (BitVector .) . (Vec.zipWith (.&.) `on` unwrap)
	(.|.) = (BitVector .) . (Vec.zipWith (.|.) `on` unwrap)

instance (Thing a, Real (BitVector a), Enum (BitVector a)) => Integral (BitVector a) where
	toInteger = vectorToInt . unwrap
	quotRem = ((fromInteger *** fromInteger) .) . on quotRem fromIntegral

instance (Thing a, Enum (BitVector a), Ord (BitVector a)) => Real (BitVector a) where
	toRational = (%1) . toInteger

instance Thing a => Enum (BitVector a) where
	toEnum = fromIntegral
	fromEnum = fromIntegral

instance Thing a => Ord (BitVector a) where
	compare = comparing toInteger

bitVectorSplit :: (Thing a, Thing b, Peano (b+b) ~ Add (Peano b) (Peano b), (b+b) ~ a) => BitVector a -> (BitVector b, BitVector b)
bitVectorSplit (BitVector v) = (BitVector.fromList *** BitVector.fromList) $ splitAt (Vec.length v `div` 2) $ Vec.toList v

fromList :: Thing a => [Bool] -> BitVector a
fromList = BitVector . Vec.fromList

toList :: Thing a => BitVector a -> [Bool]
toList = Vec.toList . unwrap

(!) :: Thing a => BitVector a -> Int -> Bool
(BitVector b) ! i = b Vec.! i

concat :: (Thing v, Thing w, Peano (v+v) ~ Add (Peano v) (Peano v), (v+v) ~ w) => BitVector v -> BitVector v -> BitVector w
concat = (BitVector .) . on Vec.concat unwrap