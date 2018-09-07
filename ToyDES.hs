{-# LANGUAGE BinaryLiterals, FlexibleContexts, TypeFamilies #-}

import Control.Arrow ((***))
import Data.List (tails, genericIndex)
import Data.Bits (xor, testBit, shift)
import BitVector

expansion :: BitVector 6 -> BitVector 8
expansion = backPermute [0,1,3,2,3,2,4,5]

expansion' :: BitVector 4 -> BitVector 8
expansion' = backPermute [4,1,2,3,2,3,4,1]

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

makeKeys' :: BitVector 10 -> [BitVector 8]
makeKeys' = take 2 . tail							-- Take the result of applying the operations 1x and 2x
			. map (p8 . combineKeyHalves)			-- Use the provided p8 permutation to make key from halves
			. iterate (leftShift *** leftShift)		-- Repeatedly left shift both key halves
			. bitVectorSplit . p10					-- Apply initial p10 permutation and then split key in half
	where
		combineKeyHalves = uncurry BitVector.concat :: (BitVector 5, BitVector 5) -> BitVector 10
		leftShift = flip shift 1

p10 :: BitVector 10 -> BitVector 10
p10 = backPermute [2,4,1,6,3,9,0,8,7,5]

p8 :: BitVector 10 -> BitVector 8
p8 = backPermute [5,2,6,3,7,4,9,8]

encrypt :: Thing keylength => BitVector 12 -> BitVector keylength -> BitVector 12
encrypt p = uncurry (flip BitVector.concat)	-- Flip and recombine l and r
			. foldl feistel					-- Apply the feistel function for each of the keys
				(bitVectorSplit p)			-- Initial value to fold: plaintext block split in half
			. makeKeys						-- Returns a rotating list of keys [0..length of DES cycle]

feistel :: (BitVector 6, BitVector 6) -> BitVector 8 -> (BitVector 6, BitVector 6)
feistel (l,r) k = (r, l `xor` (f r k))

main = print $ encrypt 0b100010110101 (0b111000111 :: BitVector 9)