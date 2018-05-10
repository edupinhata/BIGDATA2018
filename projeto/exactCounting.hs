{-
Module:	
Description:	Program that count k-cliques of a graph.
Copyright:	(c) Eduardo Pinhata
License:	GPL-3
Maintainer:	edupinhata@gmail.com


-}

import Vector
import Dados
import Text.Read

type Node		= (Integer, Integer)
type HighNeigh	= [Integer]
type Solution	= [(Node, HighNeigh)]


parseFile :: String -> [Node]
parseFile file = toNodes $ map parseLine (lines file)
	where
		parseLine l =  map toInteger (words l)
		toInteger w = do
						let maybeW = readMaybe w :: Maybe Integer
						case maybeW of
							Just w' -> w'
							Nothing -> -1
		toNodes n = map (\x -> (x!!0, x!!1)) n 


-- Transform Nodes to Solution
initSolution :: [Node] -> Solution
initSolution rawData = emit $ prepare rawData
	where 
		parmap (\x ->  (x,[])) rawData




main :: IO()
main = do
	--file <- readFile "0.edges"
	file <- readFile "3980.edges"
	let
		dataset = parseFile file
	let solution = initSolution dataset
	print( take 5 solution )		
