{-# LANGUAGE BinaryLiterals		#-}
{-# LANGUAGE DataKinds			#-}
{-# LANGUAGE FlexibleContexts	#-}
{-# LANGUAGE TypeApplications	#-}
{-# LANGUAGE TypeFamilies		#-}
{-# LANGUAGE TypeOperators		#-}

module ToyDES where

import BitVector
import Control.Arrow ((***))
import Control.Lens ((%~), both)
import Data.Bits (xor, testBit, shift)
import Data.List (tails, genericIndex)
import Data.Tuple (swap)
import GHC.TypeLits

expansion :: BitVector 6 -> BitVector 8
expansion = backPermute' [0,1,3,2,3,2,4,5]

expansion' :: BitVector 4 -> BitVector 8
expansion' = backPermute' [4,1,2,3,2,3,4,1]

f :: BitVector 6 -> BitVector 8 -> BitVector 6
f r k = BitVector.concat'		-- Apply permutation box
		$ s1 *** s2				-- Apply the substitution boxes
		$ BitVector.split		--
		$ expansion r `xor` k	--
	where
		box :: [BitVector 3] -> BitVector 4 -> BitVector 3
		box = genericIndex
		s1 = box [0b101,0b010,0b001,0b110,0b011,0b100,0b111,0b000,0b001,0b100,0b110,0b010,0b000,0b111,0b101,0b011]
		s2 = box [0b100,0b000,0b110,0b101,0b111,0b001,0b011,0b010,0b101,0b011,0b000,0b111,0b110,0b010,0b001,0b100]

f' :: BitVector 4 -> BitVector 8 -> BitVector 4
f' r =	backPermute' [1,3,2,0]	-- Apply P4 permutation
		. BitVector.concat'		-- Concat the output of the s-boxes
		. (s0 *** s1)			-- Use the s-boxes
		. BitVector.split		-- Split the result into s-box inputs
		. xor (expansion' r)	-- Expand block then XOR with key
	where
		box :: [[BitVector 2]] -> BitVector 4 -> BitVector 2
		box s i = error "Undefined"	-- TODO Define
		s0 = box [[1,0,3,2],[3,2,1,0],[0,2,1,3],[3,1,3,2]]
		s1 = box [[0,1,2,3],[2,0,1,3],[3,0,1,0],[2,1,0,3]]

makeKeys :: Thing keylength => BitVector keylength -> [BitVector 8]
makeKeys =	take 2						--
			. map (fromList . take 8)	--
			. tails . cycle . toList	--

makeKeys' :: BitVector 10 -> [BitVector 8]
makeKeys' = take 2 . tail						-- Take the result of applying the operations 1x and 2x
			. map (p8 . combineKeyHalves)		-- Use the provided p8 permutation to make key from halves
			. iterate (both %~ leftShift)		-- Repeatedly left shift both key halves
			. BitVector.split . p10				-- Apply initial p10 permutation and then split key in half
	where
		combineKeyHalves :: (BitVector 5, BitVector 5) -> BitVector 10
		combineKeyHalves = BitVector.concat'
		leftShift = flip shift 1
		p10 = backPermute' @10 @10 [2,4,1,6,3,9,0,8,7,5]
		p8 = backPermute' @10 @8 [5,2,6,3,7,4,9,8]

encrypt :: Thing keylength => BitVector 12 -> BitVector keylength -> BitVector 12
encrypt p = BitVector.concat' . swap	-- Flip and recombine l and r
			. foldl feistel				-- Apply the feistel function for each of the keys
				(BitVector.split p)		-- Initial value to fold: plaintext block split in half
			. makeKeys					-- Returns a rotating list of keys [0..length of DES cycle]

encrypt' :: Thing keylength => BitVector 12 -> BitVector keylength -> BitVector 12
encrypt' p = BitVector.concat' . swap	-- Flip and recombine l and r
			. foldl feistel				-- Apply the feistel function for each of the keys
				(BitVector.split p)		-- Initial value to fold: plaintext block split in half
			. makeKeys					-- Returns a rotating list of keys [0..length of DES cycle]

feistel :: (BitVector 6, BitVector 6) -> BitVector 8 -> (BitVector 6, BitVector 6)
feistel (l,r) k = (r, l `xor` f r k)

feistel' :: (Thing p, Thing k, (p+p) ~ k) => (BitVector p -> BitVector k -> BitVector p) -> (BitVector p, BitVector p) -> BitVector k -> (BitVector p, BitVector p)
feistel' f (l,r) k = (r, l `xor` f r k)

main = print $ encrypt 0b100010110101 (0b111000111 :: BitVector 9)