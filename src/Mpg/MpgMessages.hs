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

module Mpg.MpgMessages (
    Message(..), PlaybackState(..), readMsg, sample, PlaybackTime(..),
    Seconds(getTime), seconds) where
    
import Mpg.MpgCommands (Frames, getFrames, frames)

data Message = R String        --std_out
             | I Mp3Name       --std_out
             | S Mp3Info       --std_out
             | P PlaybackState --std_out
             ---------------------------
             | F PlaybackTime  --std_err
             | E String        --std_err
             deriving (Show)
             
type Mp3Name = String

data Mp3Info = MP {
     getVersion :: Int
   , getLayer   :: Int
   , getSamplerate :: Int
   , getMode :: String
   , getModeExtension :: Int
   , getBytesPerFrame :: Int
   , getChannelNum :: Int
   , isCopyright :: Bool
   , isCRCProtected :: Bool
   , getEmphasis :: Int
   , getBitrate :: Int
   , getExtension :: Int
   } deriving Show      

data PlaybackTime = PT {
     currentFrame    :: Frames
   , remainingFrames :: Frames
   , currentTime     :: Seconds
   , remainingTime   :: Seconds
   } deriving (Show, Eq)
   
newtype Minutes = Mins {
     getMins :: Float
   } deriving (Show, Eq)

newtype Seconds = Secs {
     getTime :: Int
   } deriving (Show, Eq, Ord)

readMsg :: String -> Maybe Message
readMsg s = let str = dropWhile (/='@') s
                match ('@':y:ys) = y
                match _          = 'X'
                ('@':x:xs)       = str
            in  case match str of
            'R' -> Just $ R $ dropWhile (==' ') xs
            'I' -> Just $ I $ dropWhile (==' ') xs
            'S' -> Just $ S $ readMp3Info xs
            'F' -> Just $ F $ readPlaybackTime xs
            'P' -> Just $ P $ playbackState $ read xs
            'E' -> Just $ E xs
            _   -> Nothing
                           
readMp3Info :: String -> Mp3Info
readMp3Info s = MP (fromIntegral $ round $ read a)
                   (read b)
                   (read c)
                   (d)
                   (read e)
                   (read f)
                   (read g)
                   (read h == 1)
                   (read i == 1)
                   (read j)
                   (read k)
                   (read l)
             where (a:b:c:d:e:f:g:h:i:j:k:l:_) = words s

readPlaybackTime :: String -> PlaybackTime
readPlaybackTime s = PT (frames $ read cF) (frames $ read rF) 
                      (seconds $ read cT) (seconds $ read rT)
                   where (cF:rF:cT:rT:_) = words s
         
minutes :: Float -> Minutes
minutes f | frac > 0.59 = minutes $ fromIntegral int +1 + frac-0.6
          | otherwise   = Mins f
          where (int,frac) = properFraction f
       
millisToMins :: Int -> Minutes
millisToMins i = minutes $ fromIntegral i / 100000

seconds :: Float -> Seconds
seconds = Secs . round

data PlaybackState = Stopped
               | Paused
               | Playing
               | Ended
               deriving (Show, Eq)

playbackState :: Int -> PlaybackState
playbackState i = case i of
                    0 -> Stopped
                    1 -> Paused
                    2 -> Playing
                    _ -> Ended 
                -- if an error occures, we assume the song has ended

sample :: [String]
sample = ["@R MPG123",
          "L Allegro from Duet in C Major.mp3",
          "@I ID3:Allegro from Duet in C Major",            
          "@S 1.0 3 44100 Joint-Stereo 2 480 2 0 0 0 192 2",
          "@F 1 2279 0.02 59.55"]

