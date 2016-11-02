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
