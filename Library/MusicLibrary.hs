module Library.MusicLibrary 
    (Library, getLibrary, getArtists,
    getArtistTracks, getAlbumTracks,
    findTrack, getAlbums, getTracklist)
    where
    
import SafeIO (FileIO, getDirectoryContents, doesDirectoryExist)
import Library.LibraryTypes
    
import qualified Data.Map.Lazy as M
import System.FilePath ((</>))
import Data.Foldable (find)
import Control.Monad (liftM, mapM, filterM)
import Data.List (isInfixOf, isSuffixOf)
import Control.Arrow (first, second)


-------- IO ---------

getLibrary :: FileIO m => FilePath -> m Library
getLibrary = liftM (loadLib . cleanLibList) . getLibList

getLibList :: FileIO m => FilePath -> m [(Artist,[(Album,Tracklist)])]
getLibList p = f'   p        >>= mapM (\x -> liftM ((,) x) $ 
               f'  (p </> x) >>= mapM (\y -> liftM ((,) y) $
               f'' (p </> x </> y) ) )
              where f   :: FileIO m => FilePath -> m [FilePath]
                    f       = liftM clean . getDirectoryContents
                    f'  :: FileIO m => FilePath -> m [FilePath]
                    f' pat  = f pat >>= filterM (doesDirectoryExist . (pat</>))
                    f'' :: FileIO m => FilePath -> m [FilePath]
                    f''     = liftM clean' . f            
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
    liftM concat (mapM (getAlbumTracks l a) albums)
    
getAlbumTracks :: Library -> Artist -> Album -> Maybe [FilePath]
getAlbumTracks l art alb = getTracklist l art alb >>=
    mapM (findTrack l art)

getArtists :: Library -> [Artist]
getArtists (Lib l) = M.keys l

getAlbums :: Library -> Artist -> Maybe [Album]
getAlbums (Lib l) art = liftM (map fst) $ M.lookup art l

getTracklist :: Library -> Artist -> Album -> Maybe Tracklist
getTracklist (Lib l) art alb = M.lookup art l >>= lookup alb

getTrackName :: Library -> (Artist, Album, Track) -> Maybe FilePath
getTrackName (Lib lib) (art, alm, trk) = M.lookup art lib >>=
                                   lookup   alm     >>=
                                   find     (isInfixOf trk) >>
                                   return   (art    </> 
                                            alm     </> trk)

findTrack :: Library -> Artist -> Track -> Maybe FilePath
findTrack (Lib lib) art trk = liftM (assembleName art trk) $
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
