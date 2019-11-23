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

module Search.TrackLibrary where
    


import Library.LibraryTypes
import SafeIO (FileIO(..))

import qualified Data.Map.Lazy as M
import System.FilePath ((</>))
import Control.Monad (liftM, mapM)
import Data.List (isInfixOf, isSuffixOf)

newtype TrackLib = TLib (M.Map Track TrackInfo)

---------------- IO ------------------

getTrackLib :: FileIO m => FilePath -> m TrackLib
getTrackLib = loadTLib . getTrLists

getTrList :: FileIO m => FilePath -> m [(Track,TrackInfo)]
getTrList p = liftM (concat . concat) $
              f'   p              >>= mapM (\x ->  
              f'  (p </> x)       >>= mapM (\y ->
              f'' (p </> x </> y) >>= (\ts -> return $
              map (g x y) ts) ) )
             where f   :: FileIO m => FilePath -> m [FilePath]
                   f       = liftM clean . getDirectoryContents
                   f'  :: FileIO m => FilePath -> m [FilePath]
                   f' pat  = f pat >>= filterM (doesDirectoryExist . (pat</>))
                   f'' :: FileIO m => FilePath -> m [FilePath]
                   f''     = fmap clean' . f   
                   g   :: Artist -> Album -> Track -> (Track,TrackInfo)
                   g a b t = (t,TrackInfo a b)  
                   clean   = filter (`notElem` [".",".."])
                   clean'  = filter (".mp3" `isSuffixOf`) . clean
                   
--------------- PURE -------------------

getTrackPath :: Track -> TrackLib -> FilePath
getTrackPath t lib = let (tName, tInfo) = head $ M.toList $ M.filterWithKey 
                                          (\k _ -> t `isInfixOf` k) lib
                     in  getArtist tInfo </> getAlbum tInfo </> tName

loadTLib :: [(Track,TrackInfo)] -> TrackLib
loadTLib = TLib . M.fromList
