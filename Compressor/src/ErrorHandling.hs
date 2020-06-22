--
-- EPITECH PROJECT, 2020
-- FUN_imageCompressor_2019
-- File description:
-- errorHandling
--

module ErrorHandling
    ( checkGoodArgs
    , exitMessage
    ) where

import System.Exit
import Text.Read
import Data.Maybe (fromMaybe)

exitMessage :: IO ()
exitMessage = do
        putStrLn "USAGE: ./imageCompressor n e IN\n"
        putStrLn "\tn\tnumber of colors in the final image"
        putStrLn "\te\tconvergence limit"
        putStrLn "\tIN\tpath to the file containing the colors of the pixels"
        exitWith (ExitFailure 84)

checkGoodArgs :: Int -> [String] -> IO()
checkGoodArgs index args
        | index > 1 = return ()
        | (readMaybe (args !! index) :: Maybe Double) == Nothing = exitMessage
        | otherwise = checkGoodArgs (index + 1) args
