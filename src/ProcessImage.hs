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

module ProcessImage ( processImages ) where

import qualified Data.Int as I 
import qualified Data.List as Dl
import qualified Data.Ord as O
import qualified Data.Vector.Storable as Dvs
import qualified Data.Word as Dw
import qualified Vision.Image as Vi
import qualified Vision.Image.Transform as Vit
import qualified Vision.Primitive as Vp
import qualified Vision.Primitive.Shape as Vps

-- It makes a small red dot on the image at the given 
-- position.
redDot :: Vi.RGB -> (Int,Int) -> Int -> Vi.RGB
redDot pic point squaresize = 
  Vi.Manifest { Vi.manifestSize = size
              , Vi.manifestVector = newvector }
  where 
    points :: [(Int,Int)]
    points = take (squaresize*squaresize) $ 
      searchPath point 1
    toShape :: [Vps.DIM2]
    toShape = [Vps.ix2 x y | (x,y) <- points]
    linearIndices :: [Int]
    linearIndices = 
      [Vps.toLinearIndex size i | i <- toShape] 
    imagevector :: Dvs.Vector Vi.RGBPixel
    imagevector = Vi.manifestVector pic
    replacements :: [(Int,Vi.RGBPixel)]
    replacements = [(pos,redPix) | pos <- linearIndices]
    size :: Vp.Size
    size = Vi.manifestSize pic
    newvector :: Dvs.Vector Vi.RGBPixel
    newvector = imagevector Dvs.// replacements

redPix :: Vi.RGBPixel
redPix = Vi.RGBPixel { Vi.rgbRed = 255
                     , Vi.rgbGreen = 0
                     , Vi.rgbBlue = 0 }

-- Given a picture and the coordinates of a point in the
-- picture, it cuts out a square piece of the picture.
detail :: Vi.RGB -> (Int,Int) -> Int -> Vi.RGB
detail pic (x,y) size = Vit.crop rect pic 
  where rect = Vp.Rect { Vp.rX = x
                       , Vp.rY = y
                       , Vp.rWidth = size
                       , Vp.rHeight = size }  

-- It searches for a small image in a big one given a 
-- starting point.
-- search :: Vi.RGB -> Vi.RGB -> (Int,Int) -> (Int,Int)
-- search small big (x,y) = (x,y)  

type Inty = (Int,(Int,Int))

searchPath :: (Int,Int) -> Int -> [(Int,Int)]
searchPath start step = no_indices
  where 
    no_indices = [x | (_,x) <- with_indices]
    with_indices = Dl.unfoldr f (0,start)
    f :: Inty -> Maybe (Inty,Inty)
    f (i,item) = 
      Just ((i,item),(i+1, (((listOfMoves step) !! i) item)))

-- It generates a sequence: [1,1,2,2,3,3,4,4,5,5...]
mvDiffs :: [Int]
mvDiffs = Dl.concatMap (replicate 2) [1..]

listOfMoves :: Int -> [(Int,Int) -> (Int,Int)]
listOfMoves step = 
  Dl.concat [take x (repeat m) | (x,m) <- zip mvDiffs mvs]
  where 
    mvs = Dl.cycle [right,up,left,down]
    right (x,y) = (x+step,y)
    up (x,y) = (x,y+step)
    left (x,y) = (x-step,y)
    down (x,y) = (x,y-step)

-- It compares two equally-sized images and works out the
-- least-squared difference between them.
compareImages :: Vi.RGB -> Vi.RGB -> I.Int64
compareImages one two = 
  bigsum (Dvs.zipWith comparePixel first second)
  where
    bigsum :: Dvs.Vector I.Int64 -> I.Int64
    bigsum = Dvs.sum 
    first = Vi.manifestVector one
    second = Vi.manifestVector two

-- It calculates the least-squared difference between two
-- RGB pixels.
comparePixel :: Vi.RGBPixel -> Vi.RGBPixel -> I.Int64
comparePixel one two = red*red + green*green + blue*blue
  where
    red = (f $ Vi.rgbRed one) - (f $ Vi.rgbRed two)
    green = (f $ Vi.rgbGreen one) - (f $ Vi.rgbGreen two)
    blue = (f $ Vi.rgbBlue one) - (f $ Vi.rgbBlue two)
    f :: Dw.Word8 -> I.Int64
    f = fromIntegral

data ProcessErr = FailedToFindAnyMatches deriving Show

processImages :: [Vi.RGB] -> Either ProcessErr [Vi.RGB]
processImages [one,two] = 
  Right [ redDot one place squaresize
        , redDot two newplace squaresize ]
  where
    place = (1500,1500)
    squaresize = 100
    newplace = fst $ search two (detail one place squaresize) 
      place squaresize

processImages _ = 
  error "You can only have two input images."

search :: Vi.RGB 
       -> Vi.RGB 
       -> (Int,Int) 
       -> Int
       -> ((Int,Int),I.Int64)
search big small start squaresize = 
  Dl.minimumBy (O.comparing snd) (take 10000 diffs)
  where
    -- for square side 50
    -- threshold = 16521950
    diffs :: [((Int,Int),I.Int64)]
    diffs = [(p, compareImages small (detail big p squaresize)) |
             p <- searchPath start 2]
