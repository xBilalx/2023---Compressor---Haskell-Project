{-
-- EPITECH PROJECT, 2024
-- compressor
-- File description:
-- Lib
-}

module Lib
    (dist,
    giveRandomElement,
    assignclustpix,
    choosekcluster,
    Color,
    Pixel,
    Posxy
    ) where

import Data.List (group, sort, maximumBy, minimumBy, nub)
import System.Random (newStdGen, randomRs)
import Data.Function (on)
import Data.Ord (comparing)
import System.Random (randomRIO)

type Posxy = (Int, Int)
type Color = (Int, Int, Int)
type Pixel = (Posxy, Color)

avvclr :: [Pixel] -> Color
avvclr pixels = meanColor $ map snd pixels

meanColor :: [Color] -> Color
meanColor colors = 
    let (rs, gs, bs) = unzip3 colors
        mean xs = sum xs `div` length xs
    in (mean rs, mean gs, mean bs)


clClosestToAverage :: Color -> [(Pixel, [Pixel])] -> Pixel
clClosestToAverage avc cl =
    fst . minimumBy (compare `on` (dist avc . snd . fst)) $ cl

dist :: Color -> Color -> Double
dist (r1, g1, b1) (r2, g2, b2) =
    sqrt $ fromIntegral ((r1 - r2)^2 + (g1 - g2)^2 + (b1 - b2)^2)

giveRandomElement :: Int -> [Pixel] -> IO [Pixel]
giveRandomElement n xs = do
    gen <- newStdGen
    let indices = take n . nub $ randomRs (0, length xs - 1) gen
    return $ map (xs !!) indices

hdpc :: [Pixel] -> Bool
hdpc p = any (\c -> length (filter (\(_, col) -> col == c) p) > 1) (map snd p)

choosekcluster :: Int -> [Pixel] -> IO [Pixel]
choosekcluster 0 _ = return []
choosekcluster k xs = do
    centroids <- if k == length xs then return xs else giveRandomElement 1 xs
    restCentroids <- choosekcluster' (k-1) xs centroids
    return $ centroids ++ restCentroids

choosekcluster' :: Int -> [Pixel] -> [Pixel] -> IO [Pixel]
choosekcluster' 0 _ _ = return []
choosekcluster' k xs centroids = do
    let distances = map (\x -> minimum $ map (dist (snd x) . snd) centroids) xs
        total = sum distances
        probs = map (/total) distances
    r <- randomRIO (0.0, 1.0)
    let centroid = snd . head . dropWhile ((<r).fst) $ zip(scanl1 (+)probs)xs
    restCentroids <- choosekcluster' (k-1) xs (centroid : centroids)
    return $ centroid : restCentroids

assignclustpix :: Double -> [Pixel] -> [Pixel] -> [(Pixel, [Pixel])]
assignclustpix convergence pixels rdm = go (map (\c -> (c, [])) rdm)
  where
    go cl =
      let newcl = assignPixelsToClusters cl pixels
      in if hasConverged convergence cl newcl
         then newcl
         else go (changeClusterCentroid newcl)

assignPixelsToClusters :: [(Pixel, [Pixel])] -> [Pixel] -> [(Pixel, [Pixel])]
assignPixelsToClusters clusters pixels =
  foldl (\acc px -> assign px acc)(map(\(center,_)->(center,[]))clusters)pixels
  where
    assign px cl =
      let closestCluster = clClosest px cl
      in map (\(center, assignedPixels) ->
                  if center == closestCluster
                  then (center, px : assignedPixels)
                  else (center, assignedPixels)) cl

clClosest :: Pixel -> [(Pixel, [Pixel])] -> Pixel
clClosest pixel = fst . minimumBy (compare `on` (dist (snd pixel) . snd . fst))

hasConverged :: Double -> [(Pixel, [Pixel])] -> [(Pixel, [Pixel])] -> Bool
hasConverged conv old new =
    let v = zipWith(\(oldCenter,_)(n,_)->dist(snd oldCenter)(snd n))old new
    in maximum v == 0

changeClusterCentroid :: [(Pixel, [Pixel])] -> [(Pixel, [Pixel])]
changeClusterCentroid clusters =
    map (\(oldCenter, pixels) ->
            if null pixels
            then error "Empty cluster"
            else
                let avc = avvclr pixels
                    newCenter = ((fst oldCenter), avc)
                in (newCenter, pixels)) clusters