module Ham.HamState 
  ( HamState(..), pTime, newState, clear
  , next, add, playing, check, shuffle, rep
  ) where
  
import Library.LibraryTypes
import Mpg.MpgMessages (PlaybackState(Playing), PlaybackTime)
import qualified System.Random as R (RandomGen, next, genRange, StdGen)

data HamState = HamState {
    getState   :: PlaybackState
  , getTrack   :: String
  , getPTime   :: PlaybackTime
  , getRepeat  :: Bool
  , getShuffle :: Bool
  , getTracks  :: Tracklist
  , prevTracks :: Tracklist
  , stateChanged :: Bool
  , getGen       :: R.StdGen
  } deriving (Show)

shuffle :: Bool -> HamState -> HamState
shuffle s state = altered $ state { getShuffle = s }

rep :: Bool -> HamState -> HamState
rep r state = altered $ state { getRepeat = r }  

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
  (t:ts) -> if  getRepeat state
              
            then case ts of
              
         [] -> (t, altered $ state { getTracks  = prevTracks state
                                   , prevTracks = t : [] } ) 
    
         _  -> if getShuffle state
                           
               then case shuffler (getGen state) (t:ts) of
       
                     (g', t', ts') -> (t', altered $ 
                                           state { getTracks   = ts'   
                                                 , prevTracks  = t': prevTracks state 
                                                 , getGen      = g' } )
                                                 
               else (t, altered $ state { getTracks  = ts
                                        , prevTracks = t: prevTracks state } )
              
            else (t, altered $ state { getTracks  = ts
                                     , prevTracks = [] } )
  _      -> if getRepeat state 
            then case prevTracks of
                 (x:xs) -> 
                 _      -> ("",state)
            else ("",state)
    

shuffler :: (R.RandomGen g, Eq a) => g -> [a] -> (g,a,[a])
shuffler g ts = (g', t, ts')
                        
                 where step   = space / (fromIntegral $ length ts)
                       space  = case R.genRange g of
                           (x, y) -> fromIntegral (y - x)
                       t   = (ts!!) $ floor (fromIntegral rv / step)
                       ts' = filter (/=t) ts
                       (rv,g') = R.next g
                       
playing :: String -> HamState -> HamState
playing track state = altered $ state { getState = Playing
                                      , getTrack = track }
                                        
altered :: HamState -> HamState
altered s = s { stateChanged = True }

check :: HamState -> HamState
check s = s  { stateChanged = False }
