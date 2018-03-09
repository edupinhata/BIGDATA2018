{-|
Module        : Exercícios Atividade 1
Description   :
Copyright     : (c) Eduardo Pinhata 
License       : GPL-3
Maintainer    : edupinhata@gmail.com

Faça uma função que recebe um tipo Integer e retorna ele dividido por 2
-}

module Main where

f :: Integer -> Double
f x = (fromIntegral x) / 2
-- A função fromIntegral tem a assinatura
-- (Num b, Integer a) => a -> b 


main :: IO()
main = do
    print(f 4)
    print(f 3)
    print(f 135)
