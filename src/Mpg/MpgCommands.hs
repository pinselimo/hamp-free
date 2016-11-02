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

--Remote Commands for MP321
module Mpg.MpgCommands (
   Volume, volume, Frames, getFrames, frames
 , JMode(..), MpgCommand(..)
 ) where

newtype Volume = V {
    getVolume :: Int
    } deriving (Show)
    
volume :: Int -> Volume
volume i | i >= 0 && i <= 100 = V i
         | i <  0             = V 0
         |           i >  100 = V 100

newtype Frames = F {
    getFrames :: Int
    } deriving (Show, Read, Eq)

frames :: Int -> Frames
frames i | i < 1     = F 1
         | otherwise = F i
 
data JMode = Plus
           | Minus

instance Show JMode where
    show Plus  = "+"
    show Minus = "-"

data MpgCommand = Jump JMode Frames -- Complex ones
             | Load FilePath
             | Gain Volume -- Simple ones
             | Pause
             | Stop
             | Quit
             
instance Show MpgCommand where
    show Stop = "S"
    show Pause = "P"
    show Quit = "Q"
    show (Gain v) = "G " ++ show (getVolume v)
    show (Load f) = "L " ++ f
    show (Jump m f) = "J " ++ show m ++ ' ': show (getFrames f)
    
