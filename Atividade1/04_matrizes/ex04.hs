{-|
Module        :
Description   :
Copyright     : (c) Eduardo Pinhata 
License       : GPL-3
Maintainer    : edupinhata@gmail.com


-}

module Main where

-- Exercício 01: Faça uma função que gere uma matriz identidade de tamanho n.
chose :: Int -> Int -> Int
chose n m
    | n == m = 1
    | otherwise = 0

array1 :: Int -> Int -> [Int]
array1 n size = [chose i n | i <- [0..size-1]]

identity :: Int -> [[Int]]
identity n = identity' (n-1) n
identity' 0 size = [array1 0 size] 
identity' n size = (identity' (n-1) size)++[array1 n size]

-- Exercício 02: Faça uma função que calcule a soma da diagonal principal de uma matriz.
getN :: [Int] -> Int -> Int
getN l 0 = head l 
getN l n  
    | (length l) <= n = getN l ((length l)-1)
    | otherwise =  getN (tail l) (n-1)

somaDiag :: [[Int]] -> Int
somaDiag m = somaDiag' m 0 0
somaDiag' [] sum _ = sum
somaDiag' (a:m') sum n = somaDiag' m' (sum+(getN a n)) (n+1)


-- Exercício 03: Faça uma função que calcule a soma da diagonal secundária de uma matriz.
somaDiagSec :: [[Int]] -> Int
somaDiagSec m = somaDiagSec' m 0 ( length m - 1)
somaDiagSec' [] sum _ = sum
somaDiagSec' (a:m') sum n = somaDiagSec' m' (sum+(getN a n)) (n-1) 
