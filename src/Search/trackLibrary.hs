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
