{-|
Module        : Atividade 1
Description   :
Copyright     : (c) Eduardo Pinhata 
License       : GPL-3
Maintainer    : edupinhata@gmail.com

Faça uma função que receba um ângulo "a" e retorne uma tupla contendo o seno 
da metade desse ângulo utilizando a identidade:

sin(x/2) = +- sqrt((1-cos(x)/2))
-}

module Main where

-- aqui não pode ser Num, nem Fractional
f :: Floating a => a -> (a, a)
f x = (x1, x2)
    where
        x1 =  tmp 
        x2 =  tmp  * (-1)
        tmp = sqrt (1 - cos(x)) / sqrt 2 


g :: (Floating a, Fractional b) => a -> a -> b
g x y = sqrt x / sqrt y

main :: IO()
main = do
    print(f (pi/2))
    print(f 1.0 )
    print(f 0.5)
    print(g 4 2)
