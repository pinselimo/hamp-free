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

module Ham.HamMessages
    (HamMessage(..), Msg(..))
    where
    
import Library.LibraryTypes
import Ham.HamState (HamState(..), getTrack)
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
