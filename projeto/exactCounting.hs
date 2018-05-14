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


-- getNeighSolEl from Index
getNeighSolEl :: Integer -> Dictionary -> NeighSolEl
getNeighSolEl x dict = ( x , dict Map.! x )


-- get tuple src
getTupleSrc :: (Edge, [Integer]) -> Integer
getTupleSrc t = fst $ fst t

getTupleDst :: (Edge, [Integer]) -> Integer
getTupleDst t = snd $ fst t

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
getAllTupleSol rawData = map (\x -> (x,[-1])) $ foldl (\x y -> x++y) [] rawData	


-- Remove elements with Neighborhood bigger than k 
filterK :: NeighSol -> Int -> NeighSol
filterK neigh k = filter (\x -> length (snd x) >= k) neigh


-- Implementation of symbol
-- NeighSolEl is Bigger than other NeighSolEl
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
filterHighNeigh :: NeighSolEl -> NeighSol -> [Integer] -> NeighSolEl
filterHighNeigh neigh [] l = (getNode neigh, l)
filterHighNeigh neigh (n:neighsList) l
	| neigh <. n = filterHighNeigh neigh neighsList (l++[(getNode n)])
	| otherwise  = filterHighNeigh neigh neighsList l


-- Find High-Neighbor of an node
getHighNeigh :: NeighSolEl -> Dictionary -> NeighSolEl
getHighNeigh neigh dict = filterHighNeigh neigh (getNeighSol neigh dict) [] 
		
	
-- get All High-Neighbor from the graph
getAllHighNeigh :: NeighSol -> Dictionary -> NeighSol
getAllHighNeigh neighs dict = map (\x -> getHighNeigh x dict) neighs

-- make a map in a list of list and concatenate 
-- the elements into a single list
flatmap :: ([a] -> [a] -> [a]) -> [[a]] -> [a]
flatmap f x = foldl f [] x 


-- transform NeighSol to TupleSol where the tuples are the 
-- cartesian product of the neighors 
neighSolToTupleSol :: NeighSol -> Dictionary -> TupleSol
neighSolToTupleSol n dict = filter cond $ foldl concat [] $ map transform n	
	where
		transform x = [((i,j), [(getNode x)]) | i <- (getNeigh x), j <- (getNeigh x)]
		concat x y = x++y 	
		cond x  = (getNeighSolEl (src x) dict) <. (getNeighSolEl (dst x) dict)
		src = getTupleSrc
		dst = getTupleDst


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
		tuples	   = getAllTupleSol dataChunks
		neighs	   = getAllNeigh dataChunks	
		highNeigh  = getAllHighNeigh neighs dict
		tuplesHighNeigh = neighSolToTupleSol highNeigh dict
		-- group the two different dataset 
		dataset'   = flatmap (\x y -> x++y) $ groupByKey (tuples ++ tuplesHighNeigh)
		-- remove the elements with -1
		dataset''  =  map (\x -> (fst x, filter (\y -> y /= -1) (snd x)) ) dataset'
		-- make the map to change from ((x,y),[u1,...,un]) to (u1, (x,y)), (u2, (x,y))...
		dataset''' = flatmap (\x y -> x++y) 
						$ map (\x -> [(i, [fst x]) | i <- (snd x)] ) dataset''  	
		-- group the elements By key	
		dataset'''' = map (\x -> foldl (\y z -> (fst y, (snd y)++(snd z))) [] x )
							$  groupByKey dataset'''	


		-- neighsLen  = map (\x -> (fst x, length (snd x) ) ) neighs
		-- highNeighLen = map (\x -> (fst x, length (snd x) ) ) highNeigh

	print ( take 7 tuples )
	print ( take 5 neighs ) 
	print ( take 5 highNeigh )
	print ( take 5 tuplesHighNeigh)
	print ( take 10 dataset' )
	print ( take 10 dataset'' )
	print ( take 10 dataset''' )
	print ( take 10 dataset'''' )


