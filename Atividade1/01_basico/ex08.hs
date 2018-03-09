{-|
Module        :
Description   :
Copyright     : (c) Eduardo Pinhata 
License       : GPL-3
Maintainer    : edupinhata@gmail.com

Crie um alista de anos bissextos desde o ano 1 até o atual.
-}

module Main where

-- Exercicio 8: Crie uma lista de anos bissextos desde o ano 1 até o atual
------------------------------------------------------------------------
ehBissexto :: Int -> Bool
ehBissexto x = (div4 || (div400 && not div100))
  where
    div4 = x `rem` 4 == 0
    div400 = x `rem` 400 == 0
    div100 = x `rem` 100 == 0


-- Imprime bissextos do ano 1 até o ano "ano"
printBiss :: Int -> [Int]
printBiss ano = [ x | x <- [1..ano], ehBissexto x ] 

-- printBiss 2018   -- imprime todos anos bissextos desde o ano 1 até 2018


-- Exercicio 9: Encontre os 10 primeiros anos bissextos
------------------------------------------------------------------------
takenFirst :: [Int] -> Int -> [Int]
takenFirst l n = take n $ l

takenLast :: [Int] -> Int -> [Int]
takenLast l n  = takeLastBiss' l ((length l) - n)
	where
		takeLastBiss' l 0 = l
		takeLastBiss' l m = takeLastBiss' (tail l) (m-1)


-- Exercício 10: Crie uma tupla em que o primeiro elemento tem metade dos anos bissextos e o segundo elemento a outra metade.
splitList :: [Int] -> ([Int], [Int])
splitList l = (l1, l2)
	where 
		l1 = takenFirst l half1
		l2 = takenLast l half2
		half1 = floor $  half
		half2 = ceiling $ half
		half = (fromIntegral (length l)) / 2

-- Conta o número de elementos de uma lista dupla
lengthDoubleList :: ([Int], [Int]) -> (Int, Int)
lengthDoubleList l = lenl
	where
		(l1, l2) = l 
		lenl = (length l1, length l2)


-- Exercício 11: Crie um concatenador de strings que concatena duas strings separadas por espaço


-- Exercício 12: Dada a string "0123456789", crie uma lista com os dígitos em formato Integer


main = do
  print( ehBissexto 2006 )
