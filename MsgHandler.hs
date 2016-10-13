module MsgHandler
 (handleMsgs, handleErrs)
 where
 
import Ham.HamState (HamState(getPTime),newState, pTime)
import Mpg.MpgMessages (Message(..), readMsg, PlaybackState(..), 
                        PlaybackTime(currentTime))
import Mpg.MpgCommands (MpgCommand(Load,Pause))

import Control.Exception (handle, SomeException, IOException)
import Control.Monad (forever)
import Control.Concurrent.MVar (takeMVar, putMVar, MVar, readMVar)
import Control.Concurrent (threadDelay)

import System.IO (hGetLine, Handle, hPutStrLn)
import qualified Data.ByteString.Char8 (hGetLine)
 
handleMsgs :: Handle -> MVar HamState -> IO ()
handleMsgs hMsg stateVar = forever $ handle ignored $ 
           handle (encodingErrors hMsg) (hGetLine hMsg) >>= \line ->
             case readMsg line of
                          
                            Just (P curr) -> takeMVar stateVar >>=
                                             putMVar  stateVar . newState curr
                                   
                            _             -> return ()
                            
                       -- >> threadDelay 1000
 
handleErrs :: Handle -> MVar HamState -> IO ()
handleErrs hErr stateVar = forever $ handle ignored $ 
           handle (encodingErrors hErr) (hGetLine hErr) >>= \line -> 
             case readMsg line of 
                            
                            Just (F pt)  -> readMVar stateVar >>= \state ->
                                if (currentTime $ getPTime state) < (currentTime pt) 
                                then takeMVar stateVar >>= 
                                     (putMVar stateVar) . (pTime pt)
                                     
                                else return ()
                                
                            Just (E or) ->  err or
                            
                            _            -> return ()
                            
                        -- >> threadDelay 1000
                        
ignored :: SomeException -> IO ()
ignored _ = threadDelay (1000*1000) >> return ()

encodingErrors :: Handle -> IOException -> IO String
encodingErrors h e = Data.ByteString.Char8.hGetLine h
                  >> return ""

--TODO implement Mpg error handling
err _ = threadDelay (1000*1000) >> return ()
