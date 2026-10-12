{-
-- EPITECH PROJECT, 2024
-- compressor
-- File description:
-- open
-}


module Openfile (splitString,strToMIntls, fileToPixelList) where
import Text.Read (readMaybe)
import Lib(Color,Pixel,Posxy)
import Data.Maybe (mapMaybe)
import Text.Printf (printf)

splitString :: Char -> String -> [String]
splitString _ [] = []
splitString sep str = go str []
  where
    go [] acc = [reverse acc]
    go (x:xs) acc
      | x `elem` "() " = go xs acc
      | x == sep  = reverse acc : go xs []
      | otherwise = go xs (x : acc)
-- isole les tuple dans la string 

spacesplit :: String -> [String]
spacesplit = go ""
  where
    go :: String -> String -> [String]
    go acc [] = [reverse acc]
    go acc (x:xs)
      | x == ' ' && null acc = go acc xs
      | x == ' '             = reverse acc : go "" xs
      | otherwise            = go (x:acc) xs

strToMIntls :: [String] -> Maybe Color
strToMIntls [x1, x2, x3]
    | Just r <- r, Just g <- g, Just b <- b,
      validComponent r && validComponent g && validComponent b = Just (r, g, b)
    | otherwise = Nothing
  where
    validComponent c = c >= 0 && c <= 255
    r = readMaybe x1
    g = readMaybe x2
    b = readMaybe x3
strToMIntls _ = Nothing
--transphorme une string en tuple int (color)

strToMpos :: [String] -> Maybe Posxy
strToMpos [x1, x2]
    | Just x <- x, Just y <- y = Just (x, y)
    | otherwise = Nothing
  where
    x = readMaybe x1
    y = readMaybe x2
strToMpos _ = Nothing

strToMPixel :: String -> Maybe Pixel
strToMPixel line =
    case spacesplit line of
        [posStr, colorStr] -> do
            pos <- strToMpos (splitString ',' posStr)
            color <- strToMIntls (splitString ',' colorStr)
            return (pos, color)
        _ -> Nothing

fileToPixelList :: String -> Maybe [Pixel]
fileToPixelList contents = do
    let pixels = Just $ mapMaybe strToMPixel (lines contents)
    case pixels of 
        Just ps | length ps == length (lines contents) -> Just ps
        _ -> Nothing