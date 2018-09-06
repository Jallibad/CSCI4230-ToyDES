import Control.Arrow
import Data.Function
import Data.Vector as Vector hiding (map, take, foldl)
import Data.List (tails, mapAccumL)
import Data.Bits hiding (xor)
import qualified Data.Bits (xor)
import qualified Data.Vector.Fixed as Vec

makeKeys :: [Int] -> [Vector Int]
makeKeys = take 2 . map (fromList . take 8) . tails . cycle

l0 :: Vector Int
l0 = vectorFromInt 6 17
r0 :: Vector Int
r0 = vectorFromInt 6 43

expansion :: Vector a -> Vector a
expansion = flip backpermute $ fromList [0,1,3,2,3,2,4,5]

encrypt :: Vector Int -> [Int] -> Vector Int
encrypt n k = uncurry (flip (Vector.++)) $ foldl (&) (Vector.splitAt 6 n) $ map feistel $ makeKeys k

xor :: Bits a => Vector a -> Vector a -> Vector a
xor = Vector.zipWith Data.Bits.xor

feistel :: Vector Int -> (Vector Int, Vector Int) -> (Vector Int, Vector Int)
feistel k (l,r) = (r, l `xor` f r k)

f :: Vector Int -> Vector Int -> Vector Int
f r k = uncurry (Vector.++) $ (s1Box *** s2Box) $ Vector.splitAt 4 (expansion r `xor` k)

s1Box :: Vector Int -> Vector Int
s1Box s = (map (vectorFromInt 3) [101,010,001,110,011,100,111,000,001,100,110,010,000,111,101,011]) !! (Vector.foldl1 (\n d -> 2*n+d) s)

s2Box :: Vector Int -> Vector Int
s2Box s = (map (vectorFromInt 3) [100,000,110,101,111,001,011,010,101,011,000,111,110,010,001,100]) !! (Vector.foldl1 (\n d -> 2*n+d) s)

vectorFromInt :: (Bits a, Num a) => Int -> a -> Vector a
vectorFromInt len n = generate len (\i -> if testBit n i then 1 else 0)

--fvectorFromInt :: (Bits a, Num a) => Int -> a -> Vec.Vector
--fvectorFromInt len n = generate len (\i -> if testBit n i then 1 else 0)