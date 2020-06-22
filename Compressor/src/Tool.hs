--
-- EPITECH PROJECT, 2020
-- Tool.hs
-- File description:
-- Tool.hs
--

module Tool
    ( randomInt
    , averageColor
    ) where

import System.Random
import System.IO.Unsafe

randomInt :: [[Int]] -> Int -> Int -> [Int]
randomInt points max1 max2
    | length points == 0 = ([value1, value2])
    | (elem [value1, value2] points) == False = ([value1, value2])
    | otherwise = randomInt points max1 max2
    where
        value1 = unsafePerformIO (getStdRandom (randomR (0, max1)))
        value2 = unsafePerformIO (getStdRandom (randomR (0, max2)))

averageColor :: Int -> Int -> Int -> Int -> Int
averageColor color divide i res
    | (color /= 0 && divide /= 0)&& i == 0 = averageColor color divide 1 (fromIntegral (color) `div` fromIntegral (divide))
    | i == 1 && ((res * 2) `mod` (res)) == 0 = (round $ fromIntegral (res))
    | i == 1 && ((res * 2) `mod` (res)) /= 0 = (floor $ fromIntegral (res))
