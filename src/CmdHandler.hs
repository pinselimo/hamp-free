module CmdHandler 
  ( handleCmds )
  where

import Mpg.MpgMessages (PlaybackTime(..), Seconds(getTime), 
                        seconds, PlaybackState(..))
import Mpg.MpgCommands (getFrames, frames, JMode(..))
import qualified Mpg.MpgCommands as Mpg (MpgCommand(Jump,Load))

import Ham.HamState    (HamState(getState, stateChanged, getPTime)
                       , next, add, playing, check, setShuffle
                       , setRepeat, clear)
import Ham.HamMessages (HamMessage(..), Msg(serialize))
import Ham.HamCommands (HamCommand(..), hamToMpg, argsToTrack, jsonToCmd)
import qualified Ham.HamCommands as Ham (HamCommand(getArtist,getAlbum,getTrack))

import Util.JSONParser (parseJSON)
import SafeIO (MesgIO(..),hGetCommandIO)
import Library.MusicLibrary (Library, getArtists,
                             getArtistTracks, getAlbumTracks,
                             findTrack, getAlbums, getTracklist)

import Data.Maybe (maybeToList)

import Control.Monad (forever, forM_, filterM, when, unless)
import Control.Concurrent.MVar (readMVar, MVar, takeMVar, putMVar)
import Control.Concurrent (threadDelay)
import Control.Exception (handle, IOException)

import System.IO.Error (isEOFError)
import System.IO (hGetContents, hGetLine, hIsClosed, Handle, hClose, hReady)
import GHC.IO.Handle (hDuplicate)
import System.Timeout (timeout)
import System.FilePath (takeFileName, dropExtension)

handleCmds :: Library -> Handle -> MVar [Handle] -> MVar HamState -> IO ()
handleCmds lib hCmd handleVar stateVar = forever $ do
        threadDelay 40000
        
        playNext hCmd stateVar
        
        takeMVar handleVar >>= \l -> do
           
           list <- filterM hIsOnline l 
           putMVar handleVar list
           
           forM_ list $ \hDev -> do
                 checkState hDev stateVar
                
                 online <- hIsOnline hDev
                 
                 when online $
                 
                    timeout 2000 (hGetCommandIO hDev) >>= \x ->
                   
                      case x of
                        Just s  -> putStrLn ("Received: " ++ s) >>
                                   react lib hCmd hDev stateVar (parseCmd s)
                        _       -> return ()

hIsOnline :: Handle -> IO Bool
hIsOnline h = handle false $ hReady h >> return True
    where false :: IOException -> IO Bool
          false e | isEOFError e = return False
                  | otherwise    = return True

parseCmd :: String -> Maybe HamCommand
parseCmd s = case parseJSON s of
    Right cmd -> jsonToCmd cmd
    _         -> Nothing

react :: Library -> Handle -> Handle -> MVar HamState -> Maybe HamCommand -> IO ()
react lib hCmd hDev stateVar Nothing    = return ()
react lib hCmd hDev stateVar (Just cmd) = case hamToMpg lib cmd of
    
    Just x  -> sendCmd hCmd $ show x
    
    _       -> case cmd of
    
        -- Special case where Mpg Command is executed here
        Jump fwd secs    -> readMVar stateVar >>= \state ->
                         sendCmd hCmd $ show $ jump (getPTime state) fwd secs
                                
        
        Play _ _ _       -> case argsToTrack lib
                                             (Ham.getArtist cmd)
                                             (Ham.getAlbum  cmd)
                                             (Ham.getTrack  cmd) of
            Just (t:ts)  -> takeMVar stateVar
                            >>= putMVar stateVar . add ts . clear
                            >>  play t hCmd stateVar
                               
            Nothing      -> return ()
        
        -- Ham Commands
        ToggleRepeat  b  -> takeMVar stateVar >>= putMVar stateVar . setRepeat b
        
        ToggleShuffle b  -> takeMVar stateVar >>= putMVar stateVar . setShuffle b
        
        RefreshLib       -> return ()
        
        GetArtists       -> send hDev $ Artists     $ getArtists lib
        
        GetAlbums artist -> send hDev $ Albums      $ concat    
                                      $ maybeToList $ getAlbums lib artist

        GetTracklist a b -> send hDev $ Tracks      $ concat
                                      $ maybeToList $ getTracklist lib a b
                                      
        GetState         -> readMVar stateVar >>= send hDev . State

        Add _ _ _        -> case argsToTrack lib 
                                             (Ham.getArtist cmd) 
                                             (Ham.getAlbum  cmd) 
                                             (Ham.getTrack  cmd) of
                                             
            Just tracks  -> takeMVar stateVar >>= putMVar stateVar . add tracks
                               
            Nothing      -> return ()

        Upload _ _ _     -> return ()
        
        _                -> return ()
  

send :: (MesgIO h m, Msg s) => h -> s -> m ()
send h s = hPutStr h (serialize s) >> hFlush h

sendCmd :: MesgIO h m => h -> String -> m ()
sendCmd h s = hPutStr h s >> hPutStr h nl >> hFlush h
    where
      nl = "\n"

jump :: PlaybackTime -> Bool -> Int -> Mpg.MpgCommand
jump pt fwd secs 
    | fwd        = Mpg.Jump Plus  jFras
    | otherwise  = Mpg.Jump Minus jFras
     where jFras = frames $ floor $ (jSecs/rSecs) * rFras
           jSecs = fromIntegral secs
           rSecs = fromIntegral $ getTime   $ remainingTime   pt
           rFras = fromIntegral $ getFrames $ remainingFrames pt

play :: String -> Handle -> MVar HamState -> IO ()
play track hCmd stateVar = takeMVar stateVar
                       >>= putMVar  stateVar . playing track
                       >>  sendCmd  hCmd (show $ Mpg.Load track)


playNext :: Handle -> MVar HamState -> IO ()
playNext hCmd m = readMVar m >>= \s -> 
        case getState s of
          Playing -> return ()
          Paused  -> return ()
            
          _       -> 
            case next s of
              ("",ns) -> return ()
               
              (t, ns) -> takeMVar m >> putMVar m ns >> play t hCmd m

checkState :: Handle -> MVar HamState -> IO ()
checkState hDev m = readMVar m >>= \s ->
    when (stateChanged s) $ send hDev (State s)
                            >> takeMVar m 
                            >> putMVar m (check s)
