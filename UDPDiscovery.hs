module UDPDiscovery (
   makeDiscoverable
 ) where

import Control.Exception (finally)
import Control.Monad (forever, forM, liftM)
import Control.Concurrent (threadDelay)
import Network.Socket hiding (listen)
import Network.BSD (hostAddress, getHostByName)
import System.Timeout (timeout)

makeDiscoverable :: PortNumber -> (String -> Bool) -> String -> IO ()
makeDiscoverable port isKey response = do
    
    sock  <- socket AF_INET Datagram defaultProtocol
    bcast <- getHostByName broadcast
    
    bind sock (SockAddrInet port $ hostAddress bcast)
    
    finally (putStrLn "Discoverable.." >> listen sock isKey response) $ close sock

listen :: Socket -> (String -> Bool) -> String -> IO ()
listen s isKey response = do
     (dat, _, ip) <- recvFrom s 1024
     if isKey dat
            then sendTo s response ip -- >> putStrLn ("Welcome " ++ show ip)
                  >> listen s isKey response
            else listen s isKey response
    
broadcast :: HostName
broadcast = "0.0.0.0"
