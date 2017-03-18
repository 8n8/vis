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

module Main where

import qualified Vision.Image as Vi
import qualified Vision.Image.Storage.DevIL as V
import qualified ProcessImage as P

-- It identifies similar points in two similar pictures.  It
-- is the first step in a program for detecting obstacles
-- in the way of a robot using the pictures from two or three
-- cameras.  This program takes in the two similar pictures
-- and outputs the same pictures with various similar points
-- marked on them.

main :: IO ()
main = do
  raw <- load froms
  let eitherRaw = sequence raw
  case eitherRaw of
    Left err -> do
      putStrLn "There was a problem loading the images."
      print err
    Right images -> do
      let eitherProcessed = P.processImages images
      case eitherProcessed of
        Left err -> do
          putStrLn "There was a problem processing \
                    \the images"
          print err
        Right processed -> do
          saved <- save tos processed
          let seqSaved = sequence saved
          case seqSaved of
            Just errs -> printErrors errs
            Nothing -> putStrLn "Completed successfully."
  where 
    froms = ["/w/git/vis/1.tiff", "/w/git/vis/2.tiff"]
    tos = ["/w/git/vis/1out.tiff", "/w/git/vis/2out.tiff"]

load :: [FilePath] -> IO [Either V.StorageError Vi.RGB]
load filepaths = 
  sequence [V.load V.Autodetect f | f <- filepaths]

printErrors :: [V.StorageError] -> IO()
printErrors errs = 
  head [putStrLn (show err) | err <- errs]

save :: [FilePath] 
     -> [Vi.RGB] 
     -> IO [Maybe V.StorageError]
save filepaths images = 
  sequence [V.save V.Autodetect f i | 
            (f,i) <- zip filepaths images ]
