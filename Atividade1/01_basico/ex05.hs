{-|
Module        : Exercícios Atividade 1
Description   : 
Copyright     : (c) Eduardo Pinhata 
License       : GPL-3
Maintainer    : edupinhata@gmail.com

Faça um programa que retorne True caso a entrada seja 
menor que -1                   OU
(maior que 1 e múltiplo de 2), E 
False caso contrário
-}

module Main where

f ::  Int -> Bool
f x
  | x < (-1)  = True
  | x > 1 &&  x `rem` 2 == 0 = True
  | otherwise =  False

main:: IO()
main = do
  print(f (-3))
  print(f 5)
  print(f 10)
