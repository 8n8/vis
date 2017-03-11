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

import qualified Codec.Picture.Repa as Cpr
import qualified Codec.Picture.Saving as Cps
import qualified Data.Array.Repa as Dar
import qualified Data.ByteString.Lazy as Db
import qualified Data.Either as De
import qualified Data.Word as Dw
import qualified Data.Array.Repa.IO.DevIL as Dev

main :: IO ()
main = readImage >>= saveImage

type EitherIm = Either String (Cpr.Img Cpr.RGB)

readImage :: Dev.IL Dev.Image
readImage = Dev.readImage "/w/git/vis/1.png"

saveImage :: EitherIm -> IO ()
saveImage = saveAsTiff . image2arr

image2arr :: EitherIm -> Arr
image2arr pic = De.either error Cpr.imgData pic

saveAsTiff :: Arr -> IO ()
saveAsTiff = 
  write . Cps.imageToPng . Cpr.imgToImage . arr2Img . 
    changeImage
  where write = Db.writeFile "/w/git/vis/op"
     
arr2Img :: Arr -> Cpr.Img Cpr.RGB
arr2Img arr = Cpr.Img {Cpr.imgData = arr}

type Arr = Dar.Array Dar.D Dar.DIM3 Dw.Word8

changeImage :: Arr -> Arr
changeImage = id
