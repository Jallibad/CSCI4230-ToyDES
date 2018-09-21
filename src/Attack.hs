{-# LANGUAGE DataKinds		#-}
{-# LANGUAGE TypeApplications	#-}

import BitVector
import Data.Bits
import Data.List (genericIndex, maximumBy)
import qualified Data.Map as Map
import Data.Ord (comparing)

box :: [[BitVector 2]] -> BitVector 4 -> BitVector 2
box s i = box' [1,2] $ box' [0,3] s
	where
		box' :: [Int] -> [a] -> a
		box' p = flip genericIndex $ backPermute' @4 @2 p i
s0 = box [[1,0,3,2],[3,2,1,0],[0,2,1,3],[3,1,3,2]]
s1 = box [[0,1,2,3],[2,0,1,3],[3,0,1,0],[2,1,0,3]]

sBox = ([3,14,1,10,4,9,5,6,8,11,15,2,13,12,0,7] !!)

count :: Ord a => [a] -> Map.Map a Int
count = foldl (\m x -> Map.insertWith (+) x 1 m) Map.empty

--maximumBy (comparing snd) $ Map.toList $ count [(x0 `xor` x1, s0 x0 `xor` s0 x1) | x0 <- [0..15], x1 <- [0..15], x0 /= x1]