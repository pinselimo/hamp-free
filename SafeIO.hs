{-# LANGUAGE FunctionalDependencies #-}
module SafeIO ( 
       BlueIO(..), Channel, Socket, Device
     , FileIO(..)
     , MesgIO(..), hGetCommandIO
     , ProcIO(..), System.Process.proc, System.Process.ProcessHandle
                 , System.IO.Handle,
     ) where
     
import Data.Word (Word8)
import Data.ByteString.Lazy.Char8 (pack)
import Data.List (isPrefixOf)
import Data.Maybe (fromMaybe)
import Control.Exception (handle, IOException)
import Control.Monad (unless)
import Control.Applicative (pure, (<$>))
import qualified Data.ByteString.Lazy as L
import System.IO.Error (isEOFError)
import qualified System.IO (hClose, hPutStr, hFlush, 
                            hPutStrLn, hGetChar, Handle)
import qualified System.Directory (getDirectoryContents, doesDirectoryExist
                        ,getCurrentDirectory, findExecutable)
                        

import qualified System.Process (createProcess, proc, 
                                ProcessHandle, CreateProcess)
     
class Monad m => FileIO m where
    doesDirectoryExist :: FilePath -> m Bool
    getDirectoryContents :: FilePath -> m [FilePath]
    getCurrentDirectory :: m FilePath

class Monad m => MesgIO h m | m -> h where
    hPutStr :: h -> String -> m ()
    hGetCommand :: h -> m String
    hClose :: h -> m ()
    hFlush :: h -> m ()
    
    hPutStrLn :: h -> String -> m ()
    hPutStrLn h s = hPutStr h s >> hPutStr h "\n"

class Monad m => BlueIO m where
    discover :: Adapter -> m [Device]
    defaultAdapter :: m (Maybe Adapter)
    deviceName :: Device -> m L.ByteString
    
    openRFCOMM :: Channel -> Device -> m Socket
    recvRFCOMM :: Socket -> Int -> m L.ByteString
    sendRFCOMM :: Socket -> L.ByteString -> m Int
    closeRFCOMM :: Socket -> m ()
    
    sendAllRFCOMM :: Socket -> L.ByteString -> m ()
    sendAllRFCOMM s bs = sendRFCOMM s bs >>= \i ->
                       unless (fromIntegral i == L.length bs)
                              (sendAllRFCOMM s $ L.drop (fromIntegral i) bs)

class Monad m => ProcIO h m | m -> h where
    findMPG321 :: m (Maybe FilePath)
    findMPG123 :: m (Maybe FilePath)
    createProcess :: System.Process.CreateProcess -> m (Maybe h, 
                                                  Maybe h,
                                                  Maybe h,
                                                  System.Process.ProcessHandle)
    

type Channel = Word8
type Device = String
type Adapter = String
type Socket = [String]

instance FileIO IO where
    doesDirectoryExist = System.Directory.doesDirectoryExist
    getDirectoryContents = System.Directory.getDirectoryContents
    getCurrentDirectory = System.Directory.getCurrentDirectory

instance MesgIO System.IO.Handle IO where
    hPutStr = System.IO.hPutStr
    hGetCommand = hGetCommandIO
    hClose = System.IO.hClose
    hFlush = System.IO.hFlush
    hPutStrLn = System.IO.hPutStrLn
    
instance ProcIO System.IO.Handle IO where
    createProcess = System.Process.createProcess
    findMPG321 = System.Directory.findExecutable "mpg321"
    findMPG123 = System.Directory.findExecutable "mpg123"
    --findWGET   = System.Directory.findExecutable "wget"

--Just for testing
instance BlueIO Maybe where
    discover = Just . pure
    defaultAdapter = Just $ Just "Adapter"
    deviceName = Just . pack

    openRFCOMM dev c = Just ["a","b","c","d","e"]
    recvRFCOMM sock i = Just $ pack $ head sock
    sendRFCOMM sock s = Just $ fromIntegral $ L.length s
    closeRFCOMM sock = Nothing

isLocalIP :: String -> Bool
isLocalIP s =  "192.168." `isPrefixOf` s
            || "10."      `isPrefixOf` s
            ||  foldr (\c b -> c `isPrefixOf` s || b) False loc
            where loc = map (\n -> "172."++show n++".") [16..31]

hGetCommandIO :: System.IO.Handle -> IO String
hGetCommandIO h = handle onError $ System.IO.hGetChar h >>= \c ->
    if c == '}' then return "}"
                else (c:) <$> hGetCommandIO h
                where onError :: IOException -> IO String
                      onError e | isEOFError e = return "}"
                                | otherwise    = hGetCommandIO h
               
blue :: BlueIO m => m String
blue = do
    adp <- defaultAdapter
    let a = fromMaybe "No adp" adp
    (x:xs) <- discover a
    sock   <- openRFCOMM 1 x
    return x
