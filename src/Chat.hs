{-# LANGUAGE BinaryLiterals		#-}
{-# LANGUAGE OverloadedStrings	#-}

module Chat where

import Control.Exception (bracket)
import Control.Monad (unless)
import Data.ByteString (ByteString, null, append)
import Data.ByteString.Char8 (putStrLn, pack)
import Data.ByteString (getLine)
import Network.Socket hiding (recv)
import Network.Socket.ByteString (recv,sendAll)
import Prelude hiding (getLine, putStrLn, null)
import ToyDES hiding (main)

main :: IO ()
main = withSocketsDo $ do
	command <- getLine
	case command of
		"listen\r" -> do
			putStrLn "listening"
			listenForChat
		"connect\r" -> connectToChat
		other -> putStrLn "invalid argument: "

listenForChat :: IO ()
listenForChat = do
	addr <- resolve "3000"
	bracket (open addr) close $ \sock -> do
		(conn, peer) <- accept sock
		--putStrLn $ append "Connection from " $ pack $ show peer
		msg <- recv conn 1024
		putStrLn msg
		sendAll conn msg
		close conn
  where
	resolve port = do
		let hints = defaultHints {
				addrFlags = [AI_PASSIVE]
			  , addrSocketType = Stream
			  }
		addr:_ <- getAddrInfo (Just hints) Nothing (Just port)
		return addr
	open addr = do
		sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
		setSocketOption sock ReuseAddr 1
		bind sock (addrAddress addr)
		listen sock 10
		return sock
	talk conn = do
		msg <- recv conn 1024
		putStrLn msg
		unless (null msg) $ do
			sendAll conn msg
			talk conn

connectToChat :: IO ()
connectToChat = do
	putStrLn "connecting"
	addr <- head <$> getAddrInfo (Just defaultHints) (Just "127.0.0.1") (Just "3000")
	bracket (open addr) close talk
	where
		open addr = do
			sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
			connect sock $ addrAddress addr
			return sock
		talk sock = do
			sendAll sock $ encrypt' 0b10110101 "Hello, world!"
			msg <- Network.Socket.ByteString.recv sock 1024
			putStr "Received: "
			putStrLn $ decrypt' 0b10110101 msg