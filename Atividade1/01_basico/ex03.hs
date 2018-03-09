{-|
Module        : Exercícios Básicos
Description   : Exercícios da Atividade 1
Copyright     : (c) Eduardo Pinhata, 2018 
License       : GPL-3
Maintainer    : edupinhata@gmail.com

Faça uma função mult5 x que retorne True caso a entrada seja
 múltiplo de 5 e False caso contrário.
-}

module Main where

mult5 :: Int -> Bool
mult5 x
    | rem x 5 == 0 = True
    | otherwise    = False

main :: IO()
main = do
    print( mult5 24 )
    print( mult5 25 )
    print( mult5 50 )
