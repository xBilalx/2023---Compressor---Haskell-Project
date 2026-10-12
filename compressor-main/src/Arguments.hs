{-
-- EPITECH PROJECT, 2024
-- compressor
-- File description:
-- arguments
-}

module Arguments(
    verifyinvocation,
    fnc,
    findnumConv,
    findpath) where
import System.IO 
import System.Exit (exitWith, ExitCode(..))
import Text.Read (readMaybe)
import System.IO.Error (catchIOError)

verifyinvocation :: [String] -> IO ()
verifyinvocation [] = return ()
verifyinvocation ("-n":val:xs) = validateInt val xs
verifyinvocation ("-l":val:xs) = validatedb val xs
verifyinvocation ("-f":val:xs) = validatePath val xs
verifyinvocation (_:_) = exitWith (ExitFailure 84)

validateInt :: String -> [String] -> IO ()
validateInt str xs=
    case reads str :: [(Int, String)] of
        [(_, "")] -> verifyinvocation xs 
        _           -> exitWith (ExitFailure 84)

validatedb :: String -> [String] -> IO ()
validatedb str xs=
    case reads str :: [(Double, String)] of
        [(_, "")] -> verifyinvocation xs 
        _           -> exitWith (ExitFailure 84)

tO :: FilePath -> IO (Maybe Handle)
tO path = catchIOError (Just <$> openFile path ReadMode) (\_ -> return Nothing)

validatePath :: FilePath -> [String] -> IO ()
validatePath path xs = do
    result <- tO path
    case result of
        Just _ -> verifyinvocation xs 
        Nothing -> exitWith (ExitFailure 84)


fnc :: [String] -> Int
fnc ("-n":value:_) = case readMaybe value of
  Just intValue -> intValue
  Nothing       -> 0
fnc (_:rest) = fnc rest
fnc [] = 0

findnumConv :: [String] -> Double 
findnumConv ("-l":value:_) = case readMaybe value of
  Just intValue -> intValue
  Nothing       -> 0
findnumConv (_:rest) = findnumConv rest
findnumConv [] = 0

findpath :: [String] -> String
findpath ("-f":value:_) = value
findpath (_:rest) = findpath rest
findpath [] = ""