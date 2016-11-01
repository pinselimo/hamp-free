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
