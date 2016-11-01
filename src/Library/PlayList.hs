module Library.PlayList
    (PlayList(PL), next)
    where
    
newtype PlayList = PL {
   getTracks :: [FilePath]
 }

next :: PlayList -> Bool -> Maybe (FilePath, PlayList)
next pl repeat 
  | repeat    = case f of
    ts@(t:_) -> Just (t,PL ts)
    _        -> Nothing

  | otherwise = case f of
    (t:ts)   -> Just (t,PL ts)
    _        -> Nothing
    
    where f = getTracks pl
 

shuffleNext :: PlayList -> Double -> Maybe FilePath
shuffleNext pl d = takeTrack n pl
    where l = length $ getTracks pl
          f = floor  $ l * d
          n = if f >= l then l-1 else f
          takeTrack x (p:ps) | x == 0    = Just p
                             | otherwise = takeTrack (x-1) ps
          takeTrack _ [] = Nothing
