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
type Edge       = (Integer, Integer)
type TupleSol   = [(Edge, [Integer])]
type NeighSolEl = (Integer, [Integer])
type NeighSol   = [NeighSolEl]
type Dictionary = Map.Map Integer [Integer]


-- get neighbors of a NeighSolEl
getNeigh :: NeighSolEl -> [Integer]
getNeigh n = snd n 


-- get Node of a NeighSolEl
getNode :: NeighSolEl -> Integer
getNode n = fst n

-- Format content read from file
parseFile :: String ->  [Edge]
parseFile file = toEdges $ map parseLine (lines file)
	where
		parseLine l = map toInteger (words l)
		toInteger w = do
						let maybeW = readMaybe w :: Maybe Integer
						case maybeW of
							Just w' -> w'
							Nothing -> -1
		toEdges n = map (\x -> (x!!0, x!!1)) n 


-- Transform Nodes to Solution
initSolution :: ChunksOf Edge -> TupleSol
initSolution rawData = prepare rawData
	where 
		prepare x = map (\x -> (x,[])) rawData


-- Get Neighborhood of each node
getAllNeigh :: ChunksOf [Edge] -> NeighSol
getAllNeigh rawData = mapReduceByKey separeSrcNode appendDst rawData
	where
		separeSrcNode x = (fst x, [snd x] )
		appendDst x y = x++y


getAllTupleSol :: ChunksOf [Edge] -> TupleSol
getAllTupleSol rawData = map (\x -> (x,[])) $ foldl (\x y -> x++y) [] rawData	


-- Remove elements with Neighborhood bigger than k 
filterK :: NeighSol -> Int -> NeighSol
filterK neigh k = filter (\x -> length (snd x) >= k) neigh


-- Implementation of symbol
(<.) :: NeighSolEl -> NeighSolEl -> Bool
(<.) x y 
	| (P.length (getNeigh x) <= P.length (getNeigh y)) && (getNode x) < (getNode y) = True
	| otherwise    = False


-- Get all the NeighSol from a neighborhood of a neighSol
getNeighSol :: NeighSolEl -> Dictionary -> NeighSol
getNeighSol n dict = map toNeighSol (getNeigh n)
	where
		toNeighSol x = (x, dict Map.! x) 


-- Filter only the elements from a NeighSol that are
-- bigger than neigh
myFilter :: NeighSolEl -> NeighSol -> [Integer] -> NeighSolEl
myFilter neigh [] l = (getNode neigh, l)
myFilter neigh (n:neighsList) l
	| neigh <. n = myFilter neigh neighsList (l++[(getNode n)])
	| otherwise  = myFilter neigh neighsList l


-- Find High-Neighbor of an node
getHighNeigh :: NeighSolEl -> Dictionary -> NeighSolEl
getHighNeigh neigh dict = myFilter neigh (getNeighSol neigh dict) [] 
		
		
	
-- get All High-Neighbor from the graph
getAllHighNeigh :: NeighSol -> Dictionary -> NeighSol
getAllHighNeigh neighs dict = map (\x -> getHighNeigh x dict) neighs


-- Create dictionary 
makeDict :: ChunksOf [Edge] -> Map.Map Integer [Integer]
makeDict dataChunks = Map.fromList (getAllNeigh dataChunks)

-- Get the High-Neighborhood for each node
--getHighNeigh :: ChunksOf [Node] -> [(Integer, [Integer])]

main :: IO()
main = do
	--file <- readFile "0.edges"
	file <- readFile "3980.edges"
	let
		dataset    = parseFile file
		dataChunks = chunksOf numCks dataset
		dict	   = makeDict dataChunks
		tSol	   = getAllTupleSol dataChunks
		neighs	   = getAllNeigh dataChunks	
		highNeigh  = getAllHighNeigh neighs dict
		neighsLen  = map (\x -> (fst x, length (snd x) ) ) neighs
		highNeighLen = map (\x -> (fst x, length (snd x) ) ) highNeigh

	print ( take 7 tSol )
	print ( take 5 neighs ) 
	print ( take 5 highNeigh )
	print ( take 5 neighsLen )
	print ( take 5 highNeighLen )
