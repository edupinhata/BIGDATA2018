{-|
Module        : Exercícios Básicos
Description   : função que verifica múltiplos
Copyright     : (c) Eduardo Pinhata 
License       : GPL-3
Maintainer    : edupinhata@gmail.com

Faça uma função mult3 que retorne True caso a entrada seja múltiplo
 de 3 e False caso contrário
-}

module Main where

mult3 :: Int -> Bool
mult3 x 
    | x `rem` 3 == 0 = True
    | otherwise = False

main = do
    print( mult3 12 )
    print( mult3 13 )
