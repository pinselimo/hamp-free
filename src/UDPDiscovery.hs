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

module UDPDiscovery (
   makeDiscoverable
 ) where

import Control.Exception (finally)
import Control.Monad (forever, forM, liftM)
import Control.Concurrent (threadDelay)
import Network.Socket hiding (listen)
--import Network.BSD (hostAddress, getHostByName)
import System.Timeout (timeout)
import Foreign.C.String (newCString, peekCString)
import Foreign.Marshal.Alloc (callocBytes, free)
import Foreign.Storable (sizeOf)

makeDiscoverable :: PortNumber -> (String -> Bool) -> String -> IO ()
makeDiscoverable port isKey response = do
    
    sock  <- socket AF_INET Datagram defaultProtocol
    addr:_ <- getAddrInfo (Nothing) (Just broadcast) (Nothing)
    -- bcast <- getHostByName broadcast
    
    bind sock (addrAddress addr) -- hostAddress bcast)
    
    finally (putStrLn "Discoverable.." >> listen sock isKey response) $ close sock

listen :: Socket -> (String -> Bool) -> String -> IO ()
listen s isKey response = do
     let recvSize = 1024
     sendPtr <- newCString response
     recvPtr <- callocBytes recvSize
     (_len, ip) <- recvBufFrom s recvPtr recvSize
     dat <- peekCString recvPtr
     free recvPtr
     if isKey dat
            then sendBufTo s sendPtr (sizeOf sendPtr) ip -- >> putStrLn ("Welcome " ++ show ip)
                  >> free sendPtr >> listen s isKey response
            else free sendPtr >> listen s isKey response
    
broadcast :: HostName
broadcast = "0.0.0.0"
