{-|
Module        : Exercícios Básicos
Description   : Exercícios da Atividade 1
Copyright     : (c) Eduardo Pinhata 
License       : GPL-3
Maintainer    : edupinhata@gmail.com

Faça uma função mult35 x que retorne True caso a entrada seja 
múltiplo de 5 e False caso contrário
-}

module Main where

mult35 :: Int -> Bool
mult35 x 
    | div 3 && div 5 = True 
    | otherwise      = False
    where
        div n = x `rem` n == 0


main = do
    print(mult35 30)
    print(mult35 19283915)
    print(mult35 35)
    print(mult35 360255)
