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

module Ham.HamCommands
    ( HamCommand(..), hamToMpg, argsToTrack, FileAddress, jsonToCmd)
    where

import qualified Mpg.MpgCommands as Mpg
import Util.JSON (JData(..), getNum, getString, getBool)
import Library.LibraryTypes hiding (getAlbum, getArtist)   
import Library.MusicLibrary (getAlbumTracks, getArtistTracks, 
                             findTrack, Library)  

import Control.Monad (liftM)
import Control.Applicative ((<$>),(<*>))

type FileAddress = String

--in order of number declaraion [0..]
data HamCommand = RefreshLib
                | Play { getTrack :: Maybe Track
                       , getAlbum :: Maybe Album
                       , getArtist :: Artist
                }
                | Stop
                | TogglePause
                | GetArtists
                | GetAlbums     Artist
                | GetTracklist  Artist Album
                | Upload        FileAddress Track TrackInfo
                | Gain          Int       
                | Jump          Bool Int
                | ToggleRepeat  Bool
                | ToggleShuffle Bool
                | Add { getTrack :: Maybe Track
                      , getAlbum :: Maybe Album
                      , getArtist :: Artist
                }
                | GetState
                | Quit
        deriving Show
        
jsonToCmd :: JData -> Maybe HamCommand
jsonToCmd (JObject cmd) = lookup "cmd" cmd >>= getNum >>= \c -> case c of
                   0    -> Just RefreshLib
                   1    -> Play (lookup "track" cmd >>= getString)
                                (lookup "album" cmd >>= getString)
                                <$> (lookup "artist" cmd >>= getString)
                   2    -> Just Stop
                   3    -> Just TogglePause
                   4    -> Just GetArtists
                   5    -> GetAlbums     <$> (lookup "artist" cmd >>= getString)
                   6    -> GetTracklist  <$> (lookup "artist" cmd >>= getString)
                                         <*> (lookup "album"  cmd >>= getString)
                   7    -> Nothing
                   8    -> Gain          <$> (lookup "num"    cmd >>= getNum)
                   9    -> Jump          <$> (lookup "bool"   cmd >>= getBool)
                                         <*> (lookup "num"    cmd >>= getNum)
                   10   -> ToggleRepeat  <$> (lookup "bool"   cmd >>= getBool)
                   11   -> ToggleShuffle <$> (lookup "bool"   cmd >>= getBool)
                   12   -> Add  (lookup "track" cmd >>= getString)
                                (lookup "album" cmd >>= getString)
                                <$> (lookup "artist" cmd >>= getString)
                   13   -> Just GetState
                   14   -> Just Quit
                   _    -> Nothing
                   
jsonToCmd _ = Nothing
          
hamToMpg :: Library -> HamCommand -> Maybe Mpg.MpgCommand
hamToMpg l cmd = case cmd of
    Stop          -> Just Mpg.Stop
    Quit          -> Just Mpg.Quit
    TogglePause   -> Just Mpg.Pause
    (Gain i)      -> Just $ Mpg.Gain $ Mpg.volume  i
    {-(Play _ _ _)  -> liftM Mpg.Load  $ argsToTrack l 
                                   (getArtist cmd) 
                                   (getAlbum  cmd) 
                                   (getTrack  cmd) 
                                   >>= safeHead-}
    _             -> Nothing
           
    
argsToTrack :: Library -> Artist -> Maybe Album -> Maybe Track -> Maybe [FilePath]
argsToTrack l artist mAlbum mTrack = case mTrack of

    Just track -> (:[]) <$> findTrack l artist track

    Nothing    -> case mAlbum of
    
      Just album -> getAlbumTracks  l artist album  
      
      Nothing    -> getArtistTracks l artist      
