-- Copyright 2017 True Ghiassi true@ghiassitrio.co.uk

-- This file is part of Vis.

-- Vis is free software: you can redistribute it
-- and/or modify it under the terms of the GNU General
-- Public License as published by the Free Software
-- Foundation, either version 3 of the License, or (at
-- your option) any later version.

-- Vis is distributed in the hope that it will be
-- useful, but WITHOUT ANY WARRANTY; without even the
-- implied warranty of MERCHANTABILITY or FITNESS FOR
-- A PARTICULAR PURPOSE.  See the GNU General Public
-- License for more details.

-- You should have received a copy of the GNU General
-- Public License along with Vis.  If not, see
-- <http://www.gnu.org/licenses/>.

-- TEMP NOTES
-- The required endpoint of this module is a function
-- that will take a large image, a small image which
-- is an approximate subset of it, and a starting point, 
-- and find the location of the small image in the large one,
-- using a spiral search from the starting point.


module Main where

import qualified Data.Either as De
import Vision.Image
import Vision.Image.Storage.DevIL (Autodetect (..), load, save, StorageError)
import Vision.Primitive (ix2)

main :: IO ()
main = do 
  pic <- load Autodetect "/w/git/vis/1.png"
  let output = processImage pic
  err <- save Autodetect "/w/git/vis/op.png" output
  errorHandler err

errorHandler :: Maybe StorageError -> IO ()
errorHandler (Just _) = 
  putStrLn "Writing to file failed, I'm afraid."
errorHandler Nothing = putStrLn "OK, it worked."

processImage = id
