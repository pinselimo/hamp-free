-- 
-- Copyright (c) 2016 Simon Plakolb
-- 
-- This program is free software; you can redistribute it and/or
-- modify it under the terms of the GNU General Public License as
-- published by the Free Software Foundation; either version 2 of
-- the License, or (at your option) any later version.
-- 
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
-- General Public License for more details.
-- 
-- You should have received a copy of the GNU General Public License
-- along with this program; if not, write to the Free Software
-- Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
-- 02111-1307, USA.
-- 

module TCPConnector
    (acceptRemotes)
    where
    
import Control.Exception (finally, handle, SomeException)
import Control.Monad (forever, filterM)
import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar (MVar, takeMVar, putMVar)
import Network (listenOn, accept, sClose, PortID(PortNumber))
import System.IO (hGetContents, hSetBuffering, BufferMode(LineBuffering)
                   , hGetLine, hIsClosed, hReady, Handle)
                    
acceptRemotes :: MVar [Handle] -> Int -> IO ()
acceptRemotes var port = do
        sock <- listenOn (PortNumber $ fromIntegral port)
        finally (forever $ do 
        --accept blocks, so we don't need delay
            (hand,ip,port) <- accept sock
            putStrLn $ "Connected to: " ++ ip ++ ':': show port
            refreshHandles var hand) (sClose sock)
        
        
refreshHandles :: MVar [Handle] -> Handle -> IO ()
refreshHandles handleVar hand = takeMVar handleVar >>=
                                putMVar handleVar . (hand:)
