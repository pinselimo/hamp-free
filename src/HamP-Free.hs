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

-- home player main file
module Main where

import Library.MusicLibrary (getLibrary, findTrack)
import SafeIO (ReadFileIO(..), ProcIO(..), proc
              ,WriteFileIO(..), Handle, ProcessHandle, hClose)
import Mpg.MpgCommands (frames, MpgCommand(Load))
import Mpg.MpgMessages (PlaybackTime(PT), seconds, PlaybackState(Ended))
import Ham.HamState (HamState(..))

import CmdHandler (handleCmds)
import UDPDiscovery (makeDiscoverable)
import TCPConnector (acceptRemotes)
import MsgHandler (handleMsgs, handleErrs)

import Control.Exception (SomeException, finally)
import Control.Concurrent (forkOS, killThread)
import Control.Concurrent.MVar (newMVar, tryTakeMVar)

import System.Environment (getArgs, withArgs)
import System.FilePath ((</>))
import System.Process (StdStream(CreatePipe), 
       std_in, std_out, std_err, cwd, waitForProcess)
import System.IO (hGetContents, hSetBuffering, BufferMode(LineBuffering)
                 ,hGetLine, hIsClosed)
import System.Timeout (timeout)
import System.Random (getStdGen, next)


main :: IO ()
main = do
    dir <- getArgs >>= parseArgs
    putStrLn $ "Initializing Library .. Collecting Songs in " ++ dir
    library <- getLibrary dir
    
    (Just hCmd,Just hMsg,Just hErr, hProc) <- startPlayer dir
    
    gain <- setupGain
    
    gen <- getStdGen
    
    handleVar <- newMVar []
    stateVar  <- newMVar $ HamState Ended initPT False False [] [] False gen
    
    let (identifier, _) = next gen --TODO devices also need human readable names
                       
    --remote discovery thread TODO random number for distinct devices
    t1 <- forkOS $ makeDiscoverable 13636
                                    (== "<discover>HamP3</discover>")
                                    "<discoverResponse>HamP3</discoverResponse>"
    
    --accepting remote thread TODO same random number
    t2 <- forkOS $ acceptRemotes handleVar 16363
       
    --remote input and control thread
    t3 <- forkOS $ handleCmds library hCmd handleVar stateVar
    
    --message output thread
    t4 <- forkOS $ handleMsgs hMsg stateVar
           
    --error output thread             
    t5 <- forkOS $ handleErrs hErr stateVar
    
    --Quit safely
    finally (waitForProcess hProc >>= print) $ do
        
        mapM_ killThread [t1,t2,t3,t4,t5]
        
        Just handles <- tryTakeMVar handleVar
        mapM_ hClose ([hCmd, hMsg, hErr] ++ handles)
        
    where initPT :: PlaybackTime
          initPT = PT f f s s
          f = frames  0
          s = seconds 0
    
handleErr :: SomeException -> IO ()
handleErr _ = return ()

setupGain :: (WriteFileIO m, ReadFileIO m) => m Int
setupGain = doesFileExist f >>= \exists ->
    if exists 
    then SafeIO.readFile f >>= \fc -> return $ read fc
    else SafeIO.writeFile f "50" >> return 50
          
          where f = ".gain"
        
parseArgs :: ReadFileIO m => [String] -> m FilePath
parseArgs ("-c":_) = getCurrentDirectory
parseArgs (f@(c:cs):fs) | c == '/'  = doesDirectoryExist f >>=
                                     \b -> if b then return f else parseArgs fs
                        | otherwise = getCurrentDirectory  >>= 
                                     \d -> let fp = d </> f in
                                               doesDirectoryExist fp >>=
                                               \b -> if b 
                                                     then return fp 
                                                     else parseArgs fs
parseArgs _        = fail "No directory found"

startPlayer :: ProcIO h m => FilePath -> 
                             m (Maybe h, Maybe h, Maybe h, ProcessHandle)
startPlayer fp = findMPG321 >>= \m -> 
               case m of 
                 Nothing -> findMPG123
                 Just fp -> return $ Just fp
               >>= \x ->
               case x of
                 Just path -> createProcess (proc path ["-R","abcd"]) {
                              cwd = Just fp, std_in = CreatePipe,
                              std_out = CreatePipe, std_err = CreatePipe 
                              }
                 _         -> fail "Executable not found"

