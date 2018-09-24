{-# LANGUAGE BinaryLiterals		#-}
{-# LANGUAGE DataKinds			#-}
{-# LANGUAGE TypeApplications	#-}

import BitVector
import Data.Bits (xor)
import Data.Function (on)
import Data.List (genericIndex, maximumBy, sortOn, groupBy)
import qualified Data.Map as Map
import Data.Ord (comparing)
import ToyDES hiding (main)

box :: [[BitVector 2]] -> BitVector 4 -> BitVector 2
box s i = box' [1,2] $ box' [0,3] s
	where
		box' :: [Int] -> [a] -> a
		box' p = flip genericIndex $ backPermute' @4 @2 p i
s0 = box [[1,0,3,2],[3,2,1,0],[0,2,1,3],[3,1,3,2]]
s1 = box [[0,1,2,3],[2,0,1,3],[3,0,1,0],[2,1,0,3]]

sBox :: Int -> Int
sBox = ([3,14,1,10,4,9,5,6,8,11,15,2,13,12,0,7] !!)

count :: Ord a => [(a,b)] -> Map.Map a [b]
count = foldr (\(x,y) -> Map.insertWith (++) x [y]) Map.empty

--maximumBy (comparing snd) $ Map.toList $ count [(x0 `xor` x1, s0 x0 `xor` s0 x1) | x0 <- [0..15], x1 <- [0..15], x0 /= x1]

differentials = [((toInteger $ x0 `xor` x1, toInteger $ s0 x0 `xor` s0 x1),(toInteger x0, toInteger x1)) | x0 <- [0..15], x1 <- [0..15], x0 /= x1]

secretKey :: BitVector 10
secretKey = 0b1110001110