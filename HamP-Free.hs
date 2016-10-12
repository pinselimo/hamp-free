-- home player main file
module Main where

import Library.MusicLibrary (getLibrary, findTrack)
import SafeIO (FileIO(..), ProcIO(..), proc, Handle, ProcessHandle, hClose)
import Mpg.MpgCommands (frames, MpgCommand(Load))
import Mpg.MpgMessages (PlaybackTime(PT), seconds, PlaybackState(Ended))
import Ham.HamState (HamState(..))

import CmdHandler (handleCmds)
import UDPDiscovery (makeDiscoverable)
import TCPConnector (acceptRemotes)
import MsgHandler (handleMsgs, handleErrs)

import Control.Monad (mzero, liftM, mapM, filterM, forM, forever)
import Control.Exception (handle, SomeException, finally)
import Control.Concurrent (forkOS, killThread, threadDelay)
import Control.Concurrent.MVar (newMVar, tryTakeMVar)

import System.Environment (getArgs, withArgs)
import System.FilePath ((</>), splitFileName)
import System.Process (StdStream(CreatePipe), 
       std_in, std_out, std_err, cwd, waitForProcess)
import System.IO (hGetContents, hSetBuffering, BufferMode(LineBuffering)
                   , hGetLine, hIsClosed)
import System.Timeout (timeout)
import System.Random (getStdGen)


main :: IO ()
main = do
    dir <- getArgs >>= parseArgs
    putStrLn $ "Initializing Library .. Collecting Songs in " ++ dir
    library <- getLibrary dir
    
    (Just hCmd,Just hMsg,Just hErr, hProc) <- startPlayer dir
    
    gen <- getStdGen --TODO maybe use own Gen
    
    handleVar <- newMVar []
    stateVar  <- newMVar $ HamState Ended "" initPT False False [] [] False gen
                       
    
    t1 <- forkOS $ makeDiscoverable (fromInteger 13636)-- ::PortNumber 
                                    (== "<discover>HamP3</discover>")
                                    ("<discoverResponse>HamP3</discoverResponse>")
    
    --accepting remote thread
    t2 <- forkOS $ acceptRemotes handleVar 16363 -- ::PortID
       
    --remote input and control thread
    t3 <- forkOS $ handleCmds library hCmd handleVar stateVar
    
    --message output thread
    t4 <- forkOS $ handleMsgs hMsg stateVar
           
    --error output thread             
    t5 <- forkOS $ handleErrs hErr stateVar
    
    --Quit safely
    finally (waitForProcess hProc >>= putStrLn . show) $ do
        
        mapM killThread [t1,t2,t3,t4,t5]
        
        Just handles <- tryTakeMVar handleVar
        mapM hClose ([hCmd, hMsg, hErr] ++ handles)
        
    where hndlr :: SomeException -> IO ()
          hndlr _ = return ()
          initPT :: PlaybackTime
          initPT = PT f f s s
          f = frames  0
          s = seconds 0
          
          
processCommand :: String -> Handle -> IO ()
processCommand cmd h = undefined
    
handleErr :: String -> IO ()
handleErr _ = return ()
        
parseArgs :: FileIO m => [String] -> m FilePath
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

startPlayer :: ProcIO h m => FilePath -> m (Maybe h, Maybe h, Maybe h, ProcessHandle)
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

