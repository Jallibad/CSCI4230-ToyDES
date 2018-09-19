{-# LANGUAGE BinaryLiterals		#-}
{-# LANGUAGE DataKinds			#-}
{-# LANGUAGE FlexibleContexts	#-}
{-# LANGUAGE TypeApplications	#-}
{-# LANGUAGE TypeFamilies		#-}
{-# LANGUAGE TypeOperators		#-}

import BitVector
import Control.Arrow ((***),(&&&))
import Control.Lens ((%~), both)
import Data.Bits (xor, testBit, shift)
import Data.List (tails, genericIndex)
import Data.Tuple (swap)
import GHC.TypeLits

expansion :: BitVector 4 -> BitVector 8
expansion = backPermute' [3,0,1,2,1,2,3,0]

f :: BitVector 4 -> BitVector 8 -> BitVector 4
f r =	backPermute' [1,3,2,0]	-- Apply P4 permutation
		. BitVector.concat'		-- Concat the output of the s-boxes
		. (s0 *** s1)			-- Use the s-boxes
		. BitVector.split		-- Split the result into s-box inputs
		. xor (expansion r)		-- Expand block then XOR with key
	where
		box :: [[BitVector 2]] -> BitVector 4 -> BitVector 2
		box s i = (s `genericIndex` backPermute' @4 @2 [0,3] i) `genericIndex` backPermute' @4 @2 [1,2] i
		s0 = box [[1,0,3,2],[3,2,1,0],[0,2,1,3],[3,1,3,2]]
		s1 = box [[0,1,2,3],[2,0,1,3],[3,0,1,0],[2,1,0,3]]

makeKeys :: BitVector 10 -> [BitVector 8]
makeKeys =	take 2 . tail							-- Take the result of applying the operations 1x and 2x
			. map (p8 . BitVector.concat' @5 @10)	-- Use the provided p8 permutation to make key from halves
			. iterate (both %~ flip shift 1)		-- Repeatedly left shift both key halves
			. BitVector.split . p10					-- Apply initial p10 permutation and then split key in half
	where
		p10 = backPermute' @10 @10 [2,4,1,6,3,9,0,8,7,5]
		p8 = backPermute' @10 @8 [5,2,6,3,7,4,9,8]

initialPermutation :: BitVector 8 -> (BitVector 4, BitVector 4)
initialPermutation = backPermute' [1,5,2,0] &&& backPermute' [3,7,4,6]
inversePermutation :: (BitVector 4, BitVector 4) -> BitVector 8
inversePermutation = backPermute' [3,0,2,4,6,1,7,5] . BitVector.concat'

encrypt :: BitVector 8 -> BitVector 10 -> BitVector 8
encrypt p = inversePermutation			-- Flip and recombine l and r
			. foldl feistel				-- Apply the feistel function for each of the keys
				(initialPermutation	p)	-- Initial value to fold: plaintext block split in half
			. makeKeys					-- Returns a rotating list of keys [0..length of DES cycle]

decrypt :: BitVector 8 -> BitVector 10 -> BitVector 8
decrypt p =	inversePermutation			-- Flip and recombine l and r
			. foldr (flip feistel)		-- Apply the feistel function for each of the keys
				(initialPermutation p)	-- Initial value to fold: plaintext block split in half
			. makeKeys					-- Returns a rotating list of keys [0..length of DES cycle]

feistel :: (BitVector 4, BitVector 4) -> BitVector 8 -> (BitVector 4, BitVector 4)
feistel (l,r) k = (r, l `xor` f r k)

main = print $ encrypt 0b10110101 0b1110001110