{-# LANGUAGE BinaryLiterals, FlexibleContexts, TypeFamilies #-}

import Control.Arrow ((***))
import Data.List (tails, genericIndex)
import Data.Bits (xor)
import BitVector

expansion :: BitVector 6 -> BitVector 8
expansion b = fromList $ map (b !) [0,1,3,2,3,2,4,5]

f :: BitVector 6 -> BitVector 8 -> BitVector 6
f r k = uncurry BitVector.concat	-- Apply permutation box
		$ (s1 *** s2)				-- Apply the substitution boxes
		$ (bitVectorSplit			:: BitVector 8 -> (BitVector 4, BitVector 4))
		$ expansion r `xor` k		--
	where
		box = genericIndex			:: [BitVector 3] -> BitVector 4 -> BitVector 3
		s1 = box [0b101,0b010,0b001,0b110,0b011,0b100,0b111,0b000,0b001,0b100,0b110,0b010,0b000,0b111,0b101,0b011]
		s2 = box [0b100,0b000,0b110,0b101,0b111,0b001,0b011,0b010,0b101,0b011,0b000,0b111,0b110,0b010,0b001,0b100]

makeKeys :: Thing keylength => BitVector keylength -> [BitVector 8]
makeKeys =	take 2						--
			. map (fromList . take 8)	--
			. tails . cycle . toList	--

encrypt :: Thing keylength => BitVector 12 -> BitVector keylength -> BitVector 12
encrypt p = uncurry (flip BitVector.concat)	-- Flip and recombine l and r
			. foldl feistel					-- Apply the feistel function for each of the keys
				(bitVectorSplit p)			-- Initial value to fold: plaintext block split in half
			. makeKeys						-- Returns a rotating list of keys [0..length of DES cycle]

feistel :: (BitVector 6, BitVector 6) -> BitVector 8 -> (BitVector 6, BitVector 6)
feistel (l,r) k = (r, l `xor` (f r k))

main = print $ encrypt 0b100010110101 (0b111000111 :: BitVector 9)