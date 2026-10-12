{-
-- EPITECH PROJECT, 2024
-- compressor
-- File description:
-- Main
-}

module Main (main) where
import Lib
import Openfile
import Arguments
import Text.Printf (printf)
import System.Exit (exitWith, ExitCode(..))
import System.Environment (getArgs)

printPixel :: Pixel -> String
printPixel ((x, y), (r, g, b)) = printf "(%d,%d) (%d,%d,%d)" x y r g b

printPixelList :: [Pixel] -> IO ()
printPixelList [] = putStrLn "Empty list"
printPixelList pixels = mapM_ (putStrLn . printPixel) pixels

printClusterAssignments :: [(Pixel, [Pixel])] -> IO ()
printClusterAssignments [] = return ()
printClusterAssignments (((_, clr), pixels):rest) = do
    putStr $ "--\n" ++ show clr ++ "\n-\n"
    printPixelList pixels
    printClusterAssignments rest

main :: IO ()
main = do
    args <- getArgs
    verifyinvocation args
    contents <- readFile (findpath args)
    let colorList = fileToPixelList contents
    case colorList of
        Nothing -> exitWith (ExitFailure 84)
        Just px -> do
            rdm <- choosekcluster (fnc args) px
            let result = assignclustpix (findnumConv args) px rdm
            printClusterAssignments result
