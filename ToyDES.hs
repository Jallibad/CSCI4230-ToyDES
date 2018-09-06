{-# LANGUAGE DataKinds, ScopedTypeVariables, FlexibleContexts, UndecidableInstances, FlexibleInstances, TypeFamilies #-}

import Control.Arrow
import Data.Function
import Data.Vector as Vector hiding (map, take, foldl)
import Data.List (tails, mapAccumL)
import Data.Bits hiding (xor)
import qualified Data.Bits (xor)
import qualified Data.Vector.Fixed as Vec
import Data.Vector.Fixed.Boxed (Vec3, Vec4)
import Data.Ratio

type Vector3 = Vector
type Vector4 = Vector
type Vector6 = Vector
type Vector8 = Vector
type Vector12 = Vector

makeKeys :: [Int] -> [Vector Int]
makeKeys = take 2 . map (fromList . take 8) . tails . cycle

l0 :: Vector Int
l0 = vectorFromInt 6 17
r0 :: Vector Int
r0 = vectorFromInt 6 43

expansion :: Vector6 a -> Vector8 a
expansion = flip backpermute $ fromList [0,1,3,2,3,2,4,5]

encrypt :: Vector12 Int -> [Int] -> Vector12 Int
encrypt n k = uncurry (flip (Vector.++)) $ foldl (&) (Vector.splitAt 6 n) $ map feistel $ makeKeys k

xor :: Bits a => Vector a -> Vector a -> Vector a
xor = Vector.zipWith Data.Bits.xor

feistel :: Vector8 Int -> (Vector6 Int, Vector6 Int) -> (Vector6 Int, Vector6 Int)
feistel k (l,r) = (r, l `xor` f r k)

f :: Vector6 Int -> Vector8 Int -> Vector6 Int
f r k = uncurry (Vector.++) $ (s1Box *** s2Box) $ Vector.splitAt 4 (expansion r `xor` k)

f' :: (Vec.Vector w Bool, Vec.Dim w ~ 6) => (Vec4 Bool, Vec4 Bool) -> w Bool
f' = uncurry Vec.concat . (box s1 *** box s2)
--f' = Vec.concat `on` box

box :: [Vec3 Bool] -> Vec4 Bool -> Vec3 Bool
box s = (s !!) . vectorToInt

s1Box :: Vector4 Int -> Vector3 Int
s1Box s = (map (vectorFromInt 3) [101,010,001,110,011,100,111,000,001,100,110,010,000,111,101,011]) !! (Vector.foldl1 (\n d -> 2*n+d) s)

s1 :: [Vec3 Bool]
s1 = [101,010,001,110,011,100,111,000,001,100,110,010,000,111,101,011]

s2Box :: Vector4 Int -> Vector3 Int
s2Box s = (map (vectorFromInt 3) [100,000,110,101,111,001,011,010,101,011,000,111,110,010,001,100]) !! (Vector.foldl1 (\n d -> 2*n+d) s)

s2 :: [Vec3 Bool]
s2 = [100,000,110,101,111,001,011,010,101,011,000,111,110,010,001,100]

vectorFromInt :: (Bits a, Num a) => Int -> a -> Vector a
vectorFromInt len n = generate len $ \i -> if testBit n i then 1 else 0

vecToVec :: Vec.Vector v Bool => Vector Int -> v Bool
vecToVec = Vec.fromList' . map (/=0) . toList

intToVector :: Vec.Vector v Bool => Int -> v Bool
intToVector n = Vec.generate $ testBit n

vectorToInt :: Integral a => Vec.Vector v Bool => v Bool -> a
vectorToInt = Vec.foldr (\d n -> 2*n + if d then 1 else 0) 0

instance Vec.Vector v Bool => Num (v Bool) where
	(+) = (fromInteger .) . on (+) vectorToInt
	(*) = (fromInteger .) . on (*) vectorToInt
	(-) = (fromInteger .) . on (-) vectorToInt
	abs = fromInteger . abs . vectorToInt
	signum _ = 1
	negate = error "Negation is not defined for boolean vectors"
	fromInteger = Vec.generate . testBit