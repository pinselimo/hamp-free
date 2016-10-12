module Ham.HamMessages
    (HamMessage(..), Msg(..))
    where
    
import Library.LibraryTypes
import Ham.HamState (HamState(..))
import Mpg.MpgMessages (PlaybackState, PlaybackTime(..), Seconds(getTime))
import Util.JSON (JData(..))

class Msg a where
    jsonize :: a -> JData
    
    serialize :: a -> String
    serialize = show . jsonize

data HamMessage = State    HamState
                | Artists  [Artist]
                | Albums   [Album]
                | Tracks   Tracklist
                | PlayList Tracklist
                
instance Msg HamMessage where
    jsonize msg = case msg of
       State ms -> JObject [("msg"  , JString "state")
                           ,("state", JString $ show $ getState ms)
                           ,("track", JString $ getTrack   ms)
                           ,("ctime", JNumber $ getTime 
                                              $ currentTime   $ getPTime ms)
                           ,("rtime", JNumber $ getTime
                                              $ remainingTime $ getPTime ms)
                           ,("repeat" , JBool $ getRepeat  ms)
                           ,("shuffle", JBool $ getShuffle ms)
                           ,("tracks", JArray $ map JString $ getTracks ms)]
                          
       Artists  a -> mesg "artists" a
       Albums   a -> mesg "albums"  a
       Tracks   a -> mesg "tracks"  a
        
mesg :: String -> [String] -> JData
mesg n d = JObject [("msg",JString n), (n, JArray $ map JString d)]
