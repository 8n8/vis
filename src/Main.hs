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

-- import qualified Data.Vector as Dv
import qualified Data.List as Dl
import qualified Data.Vector.Storable as Dvs
import qualified Data.Word as Dw
import qualified Vision.Image as Vi
import qualified Vision.Image.Storage.DevIL as V
-- import qualified Vision.Primitive.Shape as Vps

main :: IO ()
main = load >>= maybeSave >>= printError

-- It searches for a small image in a big one given a 
-- starting point.
-- search :: Vi.RGB -> Vi.RGB -> (Int,Int) -> (Int,Int)
-- search small big (x,y) = (x,y)  

searchPath :: (Int,Int) -> [(Int,Int)]
searchPath start = no_indices
  where 
    no_indices = [x | (_,x) <- with_indices]
    with_indices = Dl.unfoldr f (0,start)
    f :: (Int,(Int,Int)) -> Maybe ((Int,(Int,Int)),(Int,(Int,Int)))
    f (i,item) = Just ((i,item),(i+1, ((listOfMoves !! i) item)))

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

load :: IO (Either V.StorageError Vi.RGB)
load = V.load V.Autodetect "/w/git/vis/pinkblack2x2.tiff"

maybeSave :: Either V.StorageError Vi.RGB 
          -> IO (Maybe V.StorageError)
maybeSave (Left err) = return (Just err)
maybeSave (Right array) = save . processImage $ array

printError :: Maybe V.StorageError -> IO () 
printError (Just err) = putStrLn $ "Error: " ++ show err
printError Nothing = putStrLn "All done."

save :: Vi.RGB -> IO (Maybe V.StorageError)
save = V.save V.Autodetect "/w/git/vis/opp.png" 

processImage :: Vi.RGB -> Vi.RGB
processImage = id 
