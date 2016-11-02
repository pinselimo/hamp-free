module Library.LibraryTypes
    where

type Artist = String
type Album  = String
type Track  = String
type Tracklist = [Track]

data TrackInfo = TrackInfo {
   getArtist :: Artist
 , getAlbum  :: Album
 } deriving (Eq, Show)
