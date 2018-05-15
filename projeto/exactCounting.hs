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
type DictionaryT = Map.Map Edge [Integer]

-- get neighbors of a NeighSolEl
getNeigh :: NeighSolEl -> [Integer]
getNeigh n = snd n 


-- get Node of a NeighSolEl
getNode :: NeighSolEl -> Integer
getNode n = fst n


-- getNeighSolEl from Index
getNeighSolEl ::  Dictionary -> Integer -> NeighSolEl
getNeighSolEl dict x = ( x , dict Map.! x )


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


getAllTupleSol :: ChunksOf [Edge]-> Dictionary -> TupleSol
getAllTupleSol rawData dict = filter (\x -> (first x) <. (second x)) 
				 $ map (\x -> (x,[-1])) $ foldl (\x y -> x++y) [] rawData	
	where 
		first a = getNeighSolEl dict $ getTupleSrc a
		second a = getNeighSolEl dict $ getTupleDst a 


-- Remove elements with Neighborhood bigger than k 
filterK :: NeighSol -> Int -> NeighSol
filterK neigh k = filter (\x -> length (snd x) >= k-1) neigh


-- Implementation of symbol
-- NeighSolEl is Bigger than other NeighSolEl
(<.) :: NeighSolEl -> NeighSolEl -> Bool
(<.) x y 
	-- | (P.length (getNeigh x) <= P.length (getNeigh y)) && (getNode x) < (getNode y) = True
	| (getNode x) < (getNode y) = True
	| otherwise    = False


-- Get all the NeighSol from a neighborhood of a neighSolEl
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
neighSolToTupleSol :: NeighSol -> Dictionary -> DictionaryT -> TupleSol
neighSolToTupleSol n dict dictT = filter cond $ foldl concat [] $ map transform n	
	where
		transform x = [((i,j), [(getNode x)]) | i <- (getNeigh x), j <- (getNeigh x), isEdge (i,j) dictT ]
		concat x y = x++y	
		cond x  = (getNeighSolEl dict (src x)) <. (getNeighSolEl dict (dst x))
		src = getTupleSrc
		dst = getTupleDst


-- Create dictionary of edges
makeDict :: ChunksOf [Edge] -> Map.Map Integer [Integer]
makeDict dataChunks = Map.fromList (getAllNeigh dataChunks)


-- Create dictionary of tuples
makeDictTuple :: ChunksOf [Edge] -> Dictionary -> Map.Map Edge [Integer]
makeDictTuple dataChunks dict = Map.fromList (getAllTupleSol dataChunks dict)


-- verify if tuple is an edge
isEdge :: Edge -> DictionaryT -> Bool  
isEdge e dict = Map.member e dict


-- color line
colorLine :: String -> String
colorLine s = "\x1b[31m" ++ s ++ "\x1b[0m"


-- Get the High-Neighborhood for each node
--getHighNeigh :: ChunksOf [Node] -> [(Integer, [Integer])]

main :: IO()
main = do
	file <- readFile "0.edges"
	-- file <- readFile "test.edges"
	-- file <- readFile "3980.edges"
	let
		dataset		= parseFile file
		dataChunks	= chunksOf numCks dataset
		dict		= makeDict dataChunks
		dictT		= makeDictTuple dataChunks dict
		tuples		= getAllTupleSol dataChunks dict
		neighs		= getAllNeigh dataChunks	
		highNeigh		= getAllHighNeigh neighs dict
		filtHighNeigh	= filterK highNeigh 3
		tuplesHighNeigh = neighSolToTupleSol filtHighNeigh dict dictT
		-- group the two different dataset 
		dataset'   = flatmap (\x y -> x++y) $ groupByKey (tuples ++ tuplesHighNeigh)
		-- remove the elements with -1
		dataset''  =  map (\x -> (fst x, filter (\y -> y /= -1) (snd x)) ) dataset'
		-- make the map to change from ((x,y),[u1,...,un]) to (u1, (x,y)), (u2, (x,y))...
		dataset''' = flatmap (\x y -> x++y) 
			$ map (\x -> [(i, [fst x]) | i <- (snd x)] ) dataset''		
		-- group the elements By key	
		dataset'''' = map (\x -> foldl (\y z -> (fst z, (snd y)++(snd z))) (1,[]) x )
			$  groupByKey dataset'''	


		-- neighsLen  = map (\x -> (fst x, length (snd x) ) ) neighs
		-- highNeighLen = map (\x -> (fst x, length (snd x) ) ) highNeigh

	print ( tuples )
	putStrLn $ colorLine "\n==== Neigh:" 
	print (  neighs ) 
	putStrLn $ colorLine "\n=== HighNeigh"
	print ( highNeigh )
	putStrLn $ colorLine "\n=== Filtered HighNeigh"
	print ( filtHighNeigh )
	putStrLn $ colorLine "\n==== tuplesHighNeigh"
	print ( tuplesHighNeigh)
	putStrLn $ colorLine "\n==== group different dataset"
	print ( dataset' )
	putStrLn $ colorLine "\n==== remove elements with -1"
	print ( dataset'' )
	putStrLn $ colorLine "\n==== make from ((x,y), [u1,...,un]) to (u1, (x,y)), (u2, (x,y))..."
	print ( dataset''' )
	putStrLn $ colorLine  "\n==== group elements by key"
	print ( dataset'''' )

