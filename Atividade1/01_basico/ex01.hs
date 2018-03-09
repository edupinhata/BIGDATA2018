{-|
Module        : Exercícios Básicos
Description   : Realzar operações com o menor número possível de parênteses.
Copyright     : (c) Eduardo Pinhata 
License       : GPL-3
Maintainer    : edupinhata@gmail.com


-}

module Main where

op1 = 2*3+5
op2 = 2 + 2*3+1
op3 = 3^4+5*2^5+1

main :: IO()
main = do
  print(op1)
  print(op2)
  print(op3)
