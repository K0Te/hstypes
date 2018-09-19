{-# OPTIONS -Wall #-}

module Main where

import System.IO (openFile, hGetLine, hIsEOF, hClose, Handle)
import GHC.IO.IOMode (IOMode(..))
import Data.Vector (empty, Vector, snoc, (!), replicate, foldl', (//), fromList)
import Control.Monad (join)
import Data.List (sort, concat, sortBy)
import Data.Tuple (swap)
import System.Environment (getArgs)

import Codec.Picture
import Codec.Picture.Types ()
import GHC.Word

data Vertice = Vertice Double Double Double deriving (Show)
data Face = Face [Int] deriving (Show, Eq)

vread :: String -> Vertice
vread s = Vertice (read x) (read y) (read z)
    where s1 = tail . tail $ s
          (x, rem) = span (/= ' ') s1
          (y, rem1) = span (/= ' ') (tail rem)
          (z, _) = span (/= ' ') (tail rem1)

fread :: String -> Face
fread s = Face list
    where s1 = tail . tail $ s
          list = readFirstNum <$> (words s1)
          readFirstNum ss = read $ takeWhile (/= '/') ss

type Coord = (Int, Int)
line :: Coord -> Coord -> [Coord]
line f@(x1, y1) t@(x2, y2) = if maxd <= 1 then [f, t] else uniq $ line f middle ++ line middle t
  where dx = x2 - x1
        dy = y2 - y1
        maxd = max (abs dx) (abs dy)
        middle = (round $ (fromIntegral $ x1 + x2) / 2, round $ (fromIntegral $ y1 + y2) / 2)

uniq :: Eq a => [a] -> [a]
uniq (x1:x2:xs) = if x1 /= x2 then x1:uniq (x2:xs) else uniq (x2:xs)
uniq x = x

triangle :: Coord -> Coord -> Coord -> [Coord]
-- triangle c1 c2 c3 = foldl (\acc point -> acc ++ line c3 point) [] line_1_2
    -- where line_1_2 = line c1 c2
triangle c1 c2 c3 = concat $ [line c1 c2 | (c1, c2) <- coords]
    where [clow, cmid, cmax] = swap <$> sort (swap <$> [c1, c2, c3])
          left_line = uniq $ line cmax clow
          right_line = uniq $ line cmax cmid ++ line cmid clow
          -- coords = (\(x,y) -> (min x y, max x y)) <$> zip left_line right_line
          coords = [((x1,y1),(x2,y2))| (x1,y1)<-left_line, (x2,y2)<-right_line, y1==y2]

maxX :: Int
maxX = 640
maxY :: Int
maxY = 480
main :: IO ()
main = do
    [name] <- getArgs
    fh <- openFile name ReadMode
    (vertices, faces) <- parseFile fh empty empty
    hClose fh
    let myPlot = render vertices faces
    print vertices
    print faces
    let sorted_faces = fromList $ reverse $ sortBy (cmpZ vertices) $ foldr (\acc l -> acc:l) [] faces
    writePng "test.png" $ generateImage (\x y -> myPlot ! (x+(maxY-y-1)*maxX)) maxX maxY
    where
        cmpZ vertices (Face p1) (Face p2) = compare (indToZ p1) (indToZ p2)
            where indToZ points = maximum $ (\(Vertice x y z) -> z ) <$> (\index -> vertices ! index) <$> points

markPoints :: Vector PixelRGB8 -> [Coord] -> Word8 -> Vector PixelRGB8
markPoints plot coords light = plot // indexes
    where indexes = addPixel . toindex <$> coords
          toindex (x, y) = x + y*maxX
          addPixel index = (index, PixelRGB8 light light light)

drawFace1 :: Vector Vertice -> Vector PixelRGB8 -> (Int, Int, Int) -> Vector PixelRGB8
drawFace1 vertices plot (from, to, to2) = if visible_light > 30 then markPoints plot l visible_light else plot
    where (Vertice fx fy z1) = vertices ! (from-1)
          (Vertice tx ty z2) = vertices ! (to-1)
          (Vertice tx2 ty2 z3) = vertices ! (to2-1)
          scaleX x = floor $ (x+1)*(fromIntegral maxX)/2
          scaleY y = floor $ (y+1)*(fromIntegral maxY)/2
          l = triangle (scaleX fx, scaleY fy) (scaleX tx, scaleY ty) (scaleX tx2, scaleY ty2)
          light_vector = (0, 0, 1)
          cp (a1, a2, a3) (b1, b2, b3) = (a2*b3-a3*b2, a3*b1-a1*b3, a1*b2-a2*b1)
          dp (a1, a2, a3) (b1, b2, b3) = a1*b1 + a2*b2 + a3*b3
          -- tr_normale = cp (tx2-fx, ty2-fy, z3-z1) (tx2-tx, ty2-ty, z3-z2)
          -- tr_normale = cp (fx-tx2, fy-ty2, z1-z3) (fx-tx, fy-ty, z1-z2)
          tr_normale = cp (tx2-fx, ty2-fy, z3-z1) (tx-fx, ty-fy, z2-z1)
          normalize (x, y, z) = let len = sqrt $ x^2 + y^2 + z^2 in ((x)/len, (y)/len, (z)/len)
          coef = (normalize tr_normale) `dp` light_vector
          light = floor $ coef * 255
          visible_light = if light < 0 then 0 else light

drawFace :: Vector Vertice -> Vector PixelRGB8 -> Face -> Vector PixelRGB8
drawFace vert plot (Face vertices) = Prelude.foldl (drawFace1 vert) plot (zip3 vertices (tail vertices ++ [head vertices]) (drop 2 vertices ++ take 2 vertices))
-- (\plot -> markPoints plot (triangle (0,0) (100,100) (100,0)) 200) emptyPlot -- $ 
-- (\plot -> markPoints plot (triangle (100,100) (200,100) (150,50)) 200) emptyPlot  -- 
render :: Vector Vertice -> Vector Face -> Vector PixelRGB8
render vertices faces =  Data.Vector.foldl' (drawFace vertices) emptyPlot faces
    where emptyPlot = Data.Vector.replicate (maxX*maxY*2) (PixelRGB8 0 0 0)

parseFile :: Handle -> Vector Vertice -> Vector Face -> IO (Vector Vertice, Vector Face)
parseFile fh vertices faces = do
    eof <- hIsEOF fh
    if eof then return (vertices, faces)
    else join $ do
        ln <- hGetLine fh
        -- putStrLn line
        case take 2 ln of
            "v " -> return $ parseFile fh (vertices `snoc` (vread ln)) faces
            "f " -> return $ parseFile fh vertices (faces `snoc` (fread ln))
            _ -> return $ parseFile fh vertices faces

