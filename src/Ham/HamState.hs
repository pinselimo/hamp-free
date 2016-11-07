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

module Ham.HamState 
  ( HamState(..), pTime, newState, clear, getTrack
  , next, add, playing, check, setShuffle, setRepeat
  ) where
  
import Library.LibraryTypes
import Mpg.MpgMessages (PlaybackState(Playing), PlaybackTime)
import qualified System.Random as R (RandomGen, next, genRange, StdGen)

data HamState = HamState {
    getState   :: PlaybackState
  , getPTime   :: PlaybackTime
  , getRepeat  :: Bool
  , getShuffle :: Bool
  , getTracks  :: Tracklist
  , prevTracks :: Tracklist
  , stateChanged :: Bool
  , getGen       :: R.StdGen
  } deriving (Show)

setShuffle :: Bool -> HamState -> HamState
setShuffle s state = altered $ state { getShuffle = s }

setRepeat :: Bool -> HamState -> HamState
setRepeat r state = altered $ state { getRepeat = r }  

pTime :: PlaybackTime -> HamState -> HamState
pTime pt state = altered $ state { getPTime = pt }

newState :: PlaybackState -> HamState -> HamState
newState ps state = altered $ state { getState = ps }

add :: Tracklist -> HamState -> HamState
add tl state = altered $ state { getTracks = getTracks state ++ tl }

clear :: HamState -> HamState
clear state = altered $ state { getTracks = [], prevTracks = [] }

next :: HamState -> (Track, HamState)
next state = case getTracks state of
    
    -- as long as the tracklist contains > 1 song
    -- , we need to check if we have to shuffle
    tracks@(t:ts) -> if getShuffle state
                     
                     then case shuffle (getGen state) tracks of
       
                     (g', t', ts') -> (t', altered $ 
                                           state { getTracks   = ts'    
                                                 , getGen      = g' } )
                     
                     else (t, altered $ state { getTracks  = ts } )
    -- getTracks returns an empty list              
    _            -> if getRepeat state 
                
                    then case prevTracks state of
                    
                    (t:ts) -> (t, altered $ state { getTracks  = ts
                                                  , prevTracks = [] } )
                    _      -> ("",state)
                
                    else ("",state)
                
shuffle :: (R.RandomGen g, Eq a) => g -> [a] -> (g,a,[a])
shuffle g ts = (g', t, ts')
                        
                 where step    = space / fromIntegral (length ts)
                       space   = case R.genRange g of
                                     (x, y) -> fromIntegral (y - x)
                       t       = (ts!!) $ floor (fromIntegral rv / step)
                       ts'     = filter (/=t) ts
                       (rv,g') = R.next g
                       
playing :: String -> HamState -> HamState
playing track state = altered $ state { getState = Playing
                                      , prevTracks = track : prevTracks state }
                                        
altered :: HamState -> HamState
altered s = s { stateChanged = True }

check :: HamState -> HamState
check s = s  { stateChanged = False }

getTrack :: HamState -> Track
getTrack st  = track
 where track = case take 1 $ prevTracks st of
               (t:_)   -> t
               _       -> ""
