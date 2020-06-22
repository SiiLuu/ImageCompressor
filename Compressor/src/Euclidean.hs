--
-- EPITECH PROJECT, 2020
-- FUN_imageCompressor_2019
-- File description:
-- euclidean
--

module Euclidean
    ( euclideanDivision
    , startingPoints
    , createPoints
    , initClusters
    , displayClusters
    , updateClusters
    , findClusters
    , algoLoop
    ) where

import Tool
import Data.List
import Data.Maybe (fromMaybe)
import Text.Printf

euclideanDivision :: [Int] -> [Int] -> Int
euclideanDivision tab1 tab2 = floor (sqrt(((fromIntegral (tab1 !! 0) - fromIntegral (tab2 !! 0)) * (fromIntegral (tab1 !! 0) - fromIntegral (tab2 !! 0))) + ((fromIntegral (tab1 !! 1) - fromIntegral (tab2 !! 1)) * (fromIntegral (tab1 !! 1) - fromIntegral (tab2 !! 1))) + ((fromIntegral (tab1 !! 2) - fromIntegral (tab2 !! 2)) * (fromIntegral (tab1 !! 2) - fromIntegral (tab2 !! 2)))))

startingPoints :: [Int] -> Int -> Int -> Int
startingPoints tab index max
        | index >= length tab = max
        | (tab !! index) > max = startingPoints tab (index + 5) (tab !! index)
        | otherwise = startingPoints tab (index + 5) max

createPoints :: [Int] -> Int -> Int -> Int -> [[Int]] -> [[Int]]
createPoints tab xmax ymax nb point
        | nb > 0 = createPoints tab xmax ymax (nb -1) (point ++ [[(random !! 0), random !! 1]])
        | otherwise = point
        where
                xmax = (startingPoints tab 0 0)
                ymax = (startingPoints tab 1 0)
                random = (randomInt point xmax ymax)

findSmallestDistance :: [[Int]] -> [[Int]] -> [[Int]] -> [Int] -> Int -> Int -> Int
findSmallestDistance tabrgb points tabxy distance index i
        | index < length points = findSmallestDistance tabrgb points tabxy (distance ++ [euclideanDivision (tabrgb !! i) (tabrgb !! (fromMaybe (0) (elemIndex (points !! index) tabxy)))]) (index + 1) i
        | otherwise = (fromMaybe (0) (elemIndex (minimum distance) distance))

findClusters :: [[Int]] -> [[Int]] -> [[Int]] -> [[Int]] -> Int -> [[Int]]
findClusters tabrgb points tabxy clusters i
        | i < length tabrgb = findClusters tabrgb points tabxy (clusters ++ [r]) (i + 1)
        | otherwise = clusters
        where
                r = ([(findSmallestDistance tabrgb points tabxy [] 0 i)] ++ (tabrgb !! i))

initClusters :: [[Int]] -> [[Int]] -> [[Int]] -> [[Int]] -> Int -> [[Int]]
initClusters tabrgb points tabxy clusters i
        | i < length points = initClusters tabrgb points tabxy (clusters ++ [([i] ++ (tabrgb !! (fromMaybe (0) (elemIndex (points !! i) tabxy))))]) (i + 1)
        | otherwise = (findClusters tabrgb points tabxy clusters 0)

displayClusters :: [[Int]] -> Int -> Int -> Int -> Int -> [[Int]] -> [[Int]] -> IO()
displayClusters clusters numClusters index size head tabxy tabrgb
        | numClusters > size = putStr ""
        | index >= length clusters = displayClusters clusters (numClusters + 1) 0 size 0 tabxy tabrgb
        | numClusters == (clusters !! index !! 0) && head == 0 = do
                printf "--\n(%d,%d,%d)\n-\n" (clusters !! index !! 1) (clusters !! index !! 2) (clusters !! index !! 3)
                displayClusters clusters numClusters (index + 1) size 1 tabxy tabrgb
        | numClusters == (clusters !! index !! 0) && head == 1 = do
                printf "(%d,%d)" ((tabxy !! (fromMaybe (0) (elemIndex (drop 1 (clusters !! index)) tabrgb))) !! 0) ((tabxy !! (fromMaybe (0) (elemIndex (drop 1 (clusters !! index)) tabrgb))) !! 1)
                printf " (%d,%d,%d)\n" (clusters !! index !! 1) (clusters !! index !! 2) (clusters !! index !! 3)
                displayClusters clusters numClusters (index + 1) size 1 tabxy tabrgb
        | otherwise = displayClusters clusters numClusters (index + 1) size head tabxy tabrgb

updateClusters :: [[Int]] -> Int -> Int -> Int -> [[Int]] -> [[Int]] -> [[Int]] -> Int -> Int -> Int -> Int -> [[Int]]
updateClusters clusters numClusters index size tabxy tabrgb clusterAverage r g b divide
        | numClusters > size = (clusterAverage)
        | index >= length clusters = updateClusters clusters (numClusters + 1) 2 size tabxy tabrgb (clusterAverage ++ [[numClusters, (averageColor r divide 0 0), (averageColor g divide 0 0), (averageColor b divide 0 0)]]) 0 0 0 0
        | numClusters == (clusters !! index !! 0) = updateClusters clusters numClusters (index + 1) size tabxy tabrgb clusterAverage (r + (clusters !! index !! 1)) (g + (clusters !! index !! 2)) (b + (clusters !! index !! 3)) (divide + 1)
        | otherwise = updateClusters clusters numClusters (index + 1) size tabxy tabrgb clusterAverage r g b divide

algoLoop :: [[Int]] -> Int -> [[Int]] -> [[Int]] -> [[Int]] -> [[Int]]
algoLoop clusters x tabxy tabrgb points
        | x <= 1000 = algoLoop (findClusters tabrgb points tabxy cl 0) (x + 1) tabxy tabrgb points
        | otherwise = clusters
        where
                cl = (updateClusters clusters 0 (length points) (length points - 1) tabxy tabrgb [] 0 0 0 0)
