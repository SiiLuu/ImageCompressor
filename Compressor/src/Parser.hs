--
-- EPITECH PROJECT, 2020
-- FUN_imageCompressor_2019
-- File description:
-- parser
--

module Parser
    ( fillTab
    , fillXY
    , fillRGB
    ) where

import Text.Read

fillTab :: String -> [Int]
fillTab str = map read (words (map repChar str)) :: [Int]

fillXY :: [Int] -> [[Int]] -> Int -> Int -> [[Int]]
fillXY tab tabarnac index ind
        | (index + 5) <= length tab = fillXY tab (tabarnac ++ [[x, y]]) (index + 5) 0
        | otherwise = tabarnac
        where
                x = (tab !! index)
                y = (tab !! (index + 1))

fillRGB :: [Int] -> [[Int]] -> Int -> Int -> [[Int]]
fillRGB tab tabarnac index ind
        | (index + 5) <= (length tab + 2) = fillRGB tab (tabarnac ++ [[r, g, b]]) (index + 5) 0
        | otherwise = tabarnac
        where
                r = (tab !! index)
                g = (tab !! (index + 1))
                b = (tab !! (index + 2))

repChar :: Char -> Char
repChar ',' = ' '
repChar '(' = ' '
repChar ')' = ' '
repChar c   = c
