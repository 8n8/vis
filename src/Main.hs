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

import qualified Data.List as Dl
import qualified Data.Vector.Storable as Dvs
import qualified Data.Word as Dw
import qualified Vision.Image as Vi
import qualified Vision.Image.Storage.DevIL as V
import qualified Vision.Image.Transform as Vit
import qualified Vision.Primitive as Vp
import qualified Vision.Primitive.Shape as Vps

-- It identifies similar points in two similar pictures.  It
-- is the first step in a program for detecting obstacles
-- in the way of a robot using the pictures from two or three
-- cameras.  This program takes in the two similar pictures
-- and outputs the same pictures with various similar points
-- marked on them.

main :: IO ()
main = do
  first <- load "/w/git/vis/1.tiff"
  second <- load "/w/git/vis/2.tiff"
load >>= maybeSave >>= printError

-- It makes a small red dot on the image at the given 
-- position.
redDot :: Vi.RGB -> (Int,Int) -> Vi.RGB
redDot pic point = 
  Vi.Manifest { Vi.manifestSize = size
              , Vi.manifestVector = newvector }
  where 
    points :: [(Int,Int)]
    points = take 25 $ searchPath point 
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

searchPath :: (Int,Int) -> [(Int,Int)]
searchPath start = no_indices
  where 
    no_indices = [x | (_,x) <- with_indices]
    with_indices = Dl.unfoldr f (0,start)
    f :: Inty -> Maybe (Inty,Inty)
    f (i,item) = 
      Just ((i,item),(i+1, ((listOfMoves !! i) item)))

-- It generates a sequence: [1,1,2,2,3,3,4,4,5,5...]
mvDiffs :: [Int]
mvDiffs = Dl.concatMap (replicate 2) [1..]

listOfMoves :: [(Int,Int) -> (Int,Int)]
listOfMoves = 
  Dl.concat [take x (repeat m) | (x,m) <- zip mvDiffs mvs]
  where 
    mvs = Dl.cycle [right,up,left,down]
    right (x,y) = (x+1,y)
    up (x,y) = (x,y+1)
    left (x,y) = (x-1,y)
    down (x,y) = (x,y-1)

-- It compares two equally-sized images and works out the
-- least-squared difference between them.
compare :: Vi.RGB -> Vi.RGB -> Int
compare one two = 
  Dvs.sum (Dvs.zipWith comparePixel first second)
  where
    first = Vi.manifestVector one
    second = Vi.manifestVector two

-- It calculates the least-squared difference between two
-- RGB pixels.
comparePixel :: Vi.RGBPixel -> Vi.RGBPixel -> Int
comparePixel one two = red*red + green*green + blue*blue
  where
    red = f $ (Vi.rgbRed one) - (Vi.rgbRed two)
    green = f $ (Vi.rgbGreen one) - (Vi.rgbGreen two)
    blue = f $ (Vi.rgbBlue one) - (Vi.rgbBlue two)
    f :: Dw.Word8 -> Int
    f = fromIntegral

load :: FilePath -> IO (Either V.StorageError Vi.RGB)
load filepath= V.load V.Autodetect filepath

data TwoEithIm = ( Either V.StorageError Vi.RGB
                 , Either V.StorageError Vi.RGB )

maybeSave :: Either V.StorageError Vi.RGB 
          -> IO (Maybe V.StorageError)
maybeSave (Left err) = return (Just err)
maybeSave (Right array) = save . processImage $ array

printError :: Maybe V.StorageError -> IO () 
printError (Just err) = putStrLn $ "Error: " ++ show err
printError Nothing = putStrLn "All done."

save :: Vi.RGB -> IO (Maybe V.StorageError)
save = V.save V.Autodetect "/w/git/vis/opp.png" 

processTwoImages :: Vi.RGB -> Vi.RGB -> (Vi.RGB,
processImages = id 
