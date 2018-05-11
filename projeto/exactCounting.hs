{-
Module: 
Description:    Program that count k-cliques of a graph.
Copyright:  (c) Eduardo Pinhata
License:    GPL-3
Maintainer: edupinhata@gmail.com

-}

module Main where

import System.IO
import System.Environment
import Data.List.Split  --(chunksOf)

import qualified Data.Map as Map

import Prelude as P
import Vector
import Dados
import Text.Read

numCks = 100


-- Types
type Node       = (Integer, Integer)
type HighNeigh = [Integer]
type Solution  = [(Node, HighNeigh)]
type NeighSol  = [(Integer, [Integer])]

-- Format content read from file
parseFile :: String ->  [Node]
parseFile file = toNodes $ map parseLine (lines file)
    where
        parseLine l = map toInteger (words l)
        toInteger w = do
                        let maybeW = readMaybe w :: Maybe Integer
                        case maybeW of
                            Just w' -> w'
                            Nothing -> -1
        toNodes n = map (\x -> (x!!0, x!!1)) n 


-- Transform Nodes to Solution
initSolution :: ChunksOf Node -> Solution
initSolution rawData = prepare rawData
    where 
        prepare x = map (\x -> (x,[])) rawData


-- Get Neighborhood of each node
getNeigh :: ChunksOf [Node] -> NeighSol
getNeigh rawData = mapReduceByKey separeSrcNode appendDst rawData
    where
        separeSrcNode x = (fst x, [snd x] )
        appendDst x y = x++y


-- Remove elements with Neighborhood bigger than k 
filterK :: NeighSol -> Int -> NeighSol
filterK neigh k = filter (\x -> length (snd x) >= k) neigh


-- Implementation of symbol
(<.) :: NeighSol -> NeighSol -> Bool
(<.) x y 
    | (P.length (snd $ x!!0) <= P.length (snd $ y!!0)) && x < y = True
    | otherwise    = False


-- Find High-Neighbor
getHighNeigh :: NeighSol -> dict -> NeighSol
getHighNeigh neigh dict = filter (\x ->  


-- Create dictionary 
makeDict :: ChunksOf [Node] -> Map.Map Integer [Integer]
makeDict dataChunks = Map.fromList (getNeigh dataChunks)

-- Get the High-Neighborhood for each node
--getHighNeigh :: ChunksOf [Node] -> [(Integer, [Integer])]

main :: IO()
main = do
    --file <- readFile "0.edges"
    file <- readFile "3980.edges"
    let
        dataset = parseFile file
        dataChunks = chunksOf numCks dataset
        dict = makeDict dataChunks

    print ( dict Map.! 3982 ) 

