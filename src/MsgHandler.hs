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

module MsgHandler
 (handleMsgs, handleErrs)
 where
 
import Ham.HamState (HamState(getPTime),newState, pTime)
import Mpg.MpgMessages (Message(..), readMsg, PlaybackState(..), 
                        PlaybackTime(currentTime))
import Mpg.MpgCommands (MpgCommand(Load,Pause))

import Control.Exception (handle, SomeException, IOException)
import Control.Monad (forever, void, when)
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
                                when (currentTime (getPTime state) < currentTime pt)
                                   $ takeMVar stateVar >>= 
                                     putMVar stateVar . pTime pt
                                
                            Just (E or) ->  err or
                            
                            _            -> return ()
                            
                        -- >> threadDelay 1000
                        
ignored :: SomeException -> IO ()
ignored _ = void $ threadDelay (1000*1000)

encodingErrors :: Handle -> IOException -> IO String
encodingErrors h e = Data.ByteString.Char8.hGetLine h
                  >> return ""

--TODO implement Mpg error handling
err _ = void $ threadDelay (1000*1000)
