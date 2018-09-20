{-# LANGUAGE BinaryLiterals		#-}
{-# LANGUAGE DataKinds			#-}
{-# LANGUAGE FlexibleContexts	#-}
{-# LANGUAGE TypeApplications	#-}
{-# LANGUAGE TypeFamilies		#-}
{-# LANGUAGE TypeOperators		#-}
{-# LANGUAGE OverloadedStrings	#-}
{-# LANGUAGE ViewPatterns		#-}

import BitVector
import Control.Arrow ((***),(&&&))
import Control.Exception (bracket)
import Data.Bits (shift, xor)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B
import Data.List (genericIndex)
import Data.Tuple (swap)
import Debug.Trace
import GHC.TypeLits
import Network.Socket
import Network.Socket.ByteString (sendAll)

--main = print $ encrypt 0b10110101 0b1110001110

main :: IO ()
main = withSocketsDo $ do
    addr <- resolve "127.0.0.1" "5005"
    bracket (open addr) close talk
  where
    resolve host port = do
        let hints = defaultHints { addrSocketType = Stream }
        addr:_ <- getAddrInfo (Just hints) (Just host) (Just port)
        return addr
    open addr = do
        sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
        connect sock $ addrAddress addr
        return sock
    talk sock = do
        sendAll sock "Hello, world!"
        msg <- recv sock 1024
        putStr "Received: "
        putStrLn msg

encrypt' :: BitVector 10 -> ByteString -> ByteString
encrypt' k = B.map $ fromIntegral . flip encrypt k . fromIntegral

decrypt' :: BitVector 10 -> ByteString -> ByteString
decrypt' k = B.map $ fromIntegral . flip decrypt k . fromIntegral

encrypt :: BitVector 8 -> BitVector 10 -> BitVector 8
encrypt p = inversePerm			-- Flip and recombine l and r
			. foldl feistel		-- Apply the feistel function for each of the keys
				(initialPerm p)	-- Initial value to fold: plaintext block split in half
			. makeKeys			-- Returns a rotating list of keys [0..length of DES cycle]

decrypt :: BitVector 8 -> BitVector 10 -> BitVector 8
decrypt p =	inversePerm				-- Flip and recombine l and r
			. foldr (flip feistel)	-- Apply the feistel function for each of the keys
				(initialPerm p)		-- Initial value to fold: plaintext block split in half
			. makeKeys				-- Returns a rotating list of keys [0..length of DES cycle]

initialPerm :: BitVector 8 -> (BitVector 4, BitVector 4)
initialPerm = backPermute' [1,5,2,0] &&& backPermute' [3,7,4,6]

inversePerm :: (BitVector 4, BitVector 4) -> BitVector 8
inversePerm = backPermute' [7,4,6,0,2,5,3,1] . BitVector.concat'

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
		box s i = box' [1,2] $ box' [0,3] s
			where
				box' :: [Int] -> [a] -> a
				box' p = flip genericIndex $ backPermute' @4 @2 p i
		s0 = box [[1,0,3,2],[3,2,1,0],[0,2,1,3],[3,1,3,2]]
		s1 = box [[0,1,2,3],[2,0,1,3],[3,0,1,0],[2,1,0,3]]

makeKeys :: BitVector 10 -> [BitVector 8]
makeKeys =	take 2 . tail					-- Take the result of applying the operations 1x and 2x
			. map (p8 . BitVector.concat')	-- Use the provided p8 permutation to make key from halves
			. iterate (lShift *** lShift)	-- Repeatedly left shift both key halves
			. BitVector.split . p10			-- Apply initial p10 permutation and then split key in half
	where
		p10 = backPermute' @10 @10 [2,4,1,6,3,9,0,8,7,5]
		p8 = backPermute' @10 @8 [5,2,6,3,7,4,9,8]
		lShift :: BitVector 5 -> BitVector 5
		lShift = flip shift 1

feistel :: (BitVector 4, BitVector 4) -> BitVector 8 -> (BitVector 4, BitVector 4)
feistel (l,r) k = (r, l `xor` f r k)

test n = decrypt (encrypt n k) k == n
		where
			--n = 0b10110101
			k = 0b1110001110