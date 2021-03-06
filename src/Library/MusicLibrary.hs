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

module Library.MusicLibrary 
    (Library, getLibrary, getArtists,
    getArtistTracks, getAlbumTracks,
    findTrack, getAlbums, getTracklist)
    where
    
import SafeIO (ReadFileIO, getDirectoryContents, doesDirectoryExist)
import Library.LibraryTypes
    
import qualified Data.Map.Lazy as M
import System.FilePath ((</>))
import Data.Foldable (find)
import Control.Monad (mapM, filterM)
import Control.Applicative ((<$>))
import Data.List (isInfixOf, isSuffixOf)
import Control.Arrow (first, second)


-------- IO ---------

getLibrary :: ReadFileIO m => FilePath -> m Library
getLibrary = fmap (loadLib . cleanLibList) . getLibList

getLibList :: ReadFileIO m => FilePath -> m [(Artist,[(Album,Tracklist)])]
getLibList p = f'   p        >>= mapM (\x -> fmap ((,) x) $ 
               f'  (p </> x) >>= mapM (\y -> fmap ((,) y) $
               f'' (p </> x </> y) ) )
              where f   :: ReadFileIO m => FilePath -> m [FilePath]
                    f       = fmap clean . getDirectoryContents
                    f'  :: ReadFileIO m => FilePath -> m [FilePath]
                    f' pat  = f pat >>= filterM (doesDirectoryExist . (pat</>))
                    f'' :: ReadFileIO m => FilePath -> m [FilePath]
                    f''     = fmap clean' . f            
                    clean   = filter (`notElem` [".",".."])
                    clean'  = filter (".mp3" `isSuffixOf`) . clean


------- PURE --------

newtype Library = Lib (M.Map Artist [(Album,Tracklist)])

loadLib :: [(Artist,[(Album,Tracklist)])] -> Library
loadLib = Lib . M.fromList


cleanLibList :: [(Artist,[(Album,Tracklist)])] -> [(Artist,[(Album,Tracklist)])]
cleanLibList = g . map (second g)
      where g = foldr f []
            f (_,[]) b = b
            f a      b = a : b

getArtistTracks :: Library -> Artist -> Maybe [FilePath]
getArtistTracks l a = getAlbums l a >>= \albums -> 
    concat <$> mapM (getAlbumTracks l a) albums
    
getAlbumTracks :: Library -> Artist -> Album -> Maybe [FilePath]
getAlbumTracks l art alb = getTracklist l art alb >>=
    mapM (findTrack l art)

getArtists :: Library -> [Artist]
getArtists (Lib l) = M.keys l

getAlbums :: Library -> Artist -> Maybe [Album]
getAlbums (Lib l) art = map fst <$> M.lookup art l

getTracklist :: Library -> Artist -> Album -> Maybe Tracklist
getTracklist (Lib l) art alb = M.lookup art l >>= lookup alb

getTrackName :: Library -> (Artist, Album, Track) -> Maybe FilePath
getTrackName (Lib lib) (art, alm, trk) = M.lookup art lib >>=
                                   lookup   alm     >>=
                                   find     (isInfixOf trk) >>
                                   return   (art    </> 
                                            alm     </> trk)

findTrack :: Library -> Artist -> Track -> Maybe FilePath
findTrack (Lib lib) art trk = fmap (assembleName art trk) $
                        M.lookup art lib >>= findTrack' trk
          where findTrack' :: Track -> [(Album,Tracklist)] -> Maybe Album
                findTrack' tk ((a,ts):as) | tk `isIn` ts = Just a
                                          | otherwise    = findTrack' tk as
                findTrack' _  []                         = Nothing
                isIn :: Track -> Tracklist -> Bool
                isIn n (t:ts) = n `isInfixOf` t || isIn n ts
                isIn _ []     = False
                assembleName :: Artist -> Track -> Album -> FilePath
                assembleName a t b = a </> b </> t
