--
-- Copyright (c) 2016 Simon Plakolb
--
--  This program is free software: you can redistribute it and/or modify
--  it under the terms of the GNU Affero General Public License as published by
--  the Free Software Foundation, either version 3 of the License, or
--  (at your option) any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU Affero General Public License for more details.
--
--  You should have received a copy of the GNU Affero General Public License
--  along with this program.  If not, see <http://www.gnu.org/licenses/>
--

module TCPConnector
    (acceptRemotes)
    where
    
import Control.Exception (finally, handle, SomeException)
import Control.Monad (forever, filterM)
import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar (MVar, takeMVar, putMVar)
import Network.Socket (listen, accept, close, bind, socket, defaultHints, getAddrInfo, socketToHandle,
        AddrInfoFlag(AI_NUMERICHOST, AI_NUMERICSERV), SocketType(Stream),
        addrFlags, addrSocketType, addrFamily, addrSocketType, addrProtocol, addrAddress)
import System.IO (hGetContents, hSetBuffering, BufferMode(LineBuffering)
                   , hGetLine, hIsClosed, hReady, Handle, IOMode(ReadWriteMode))

acceptRemotes :: MVar [Handle] -> Int -> IO ()
acceptRemotes var port = do
        let hints = defaultHints { addrFlags = [AI_NUMERICHOST, AI_NUMERICSERV], addrSocketType = Stream }
        addr:_ <- getAddrInfo (Just hints) (Just "127.0.0.1") (Just $ show port)
        sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
        Network.Socket.bind sock (addrAddress addr)
        Network.Socket.listen sock 1
        finally (forever $ do 
        --accept blocks, so we don't need delay
            (socket', addr) <- Network.Socket.accept sock
            hand <- socketToHandle socket' ReadWriteMode
            close socket'
            putStrLn $ "Connected to: " ++ (show addr)
            refreshHandles var hand) (close sock)
        
        
refreshHandles :: MVar [Handle] -> Handle -> IO ()
refreshHandles handleVar hand = takeMVar handleVar >>=
                                putMVar handleVar . (hand:)
