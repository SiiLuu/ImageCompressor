--
-- EPITECH PROJECT, 2020
-- FUN_wolfram_2019
-- File description:
-- main
--

module Main where

import ErrorHandling
import Tool
import Euclidean
import Parser
import System.Environment
import Control.Monad
import System.Directory

main :: IO ()
main = do
        args <- getArgs
        when ((length args) /= 3) $ exitMessage
        checkGoodArgs 0 args
        file <- doesFileExist (args !! 2)
        when (file == False) $ exitMessage
        str <- readFile (args !! 2)
        let tab = fillTab str
        let points = createPoints tab 0 0 (read (args !! 0) :: Int) []
        let tabxy = fillXY tab [] 0 0
        let tabrgb = fillRGB tab [] 2 0
        let clusters = initClusters tabrgb points tabxy [] 0
        let finalClusters = algoLoop clusters 0 tabxy tabrgb points

        displayClusters finalClusters 0 0 (length points - 1) 0 tabxy tabrgb
