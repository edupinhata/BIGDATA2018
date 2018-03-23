{-
    Eduardo Pinhata
    22/Mar/2018
-}


-- Exercício 01: Crie uma função divisivel20 x que retorna verdadeiro se x for divisível por todos os números de 1 a 20.
divisivel20 :: Integer -> Bool
divisivel20 x = foldl (&&) True [ (rem x i) == 0 | i <- [1..5]]


-- Exercício 02: Crie uma função projectEuler5 que retorna o primeiro número natural que retorna True para a função do
-- exercício anterior. Pense em como reduzir o custo computacional.
divisivel ::  Integer -> [Int]  -> Bool
divisivel x l = foldl (||) True [ (rem x i) == 0 | i <- l ]

removeDivisor :: [Int] -> [Int] -> Int -> [Int]
removeDivisor ldst lsrc 0 = ldst
removeDivisor ldst lsrc size
    | 

projectEuler5 :: Integer
projectEuler5 = firstDiv 1
    where
        firstDiv n 
            | divisivel20 n == True = n
            | otherwise = firstDiv (n+1)

-- Exercício 03: Crie a lista de números de Fibonacci utilizando uma função geradora.

-- Exercício 04: Utilizando a lista anterior, calcule a soma dos números de Fibonacci pares dos valores que não excedem 4.000.000. (Project Euler 2)

-- Exercício 05: Faça uma função para calcular o produto escalar entre dois vetores.

--Exercício 06: Crie a função collatz x que retorna x/2, se x for par e (3x+1) se for ímpar.

-- Exercício 07: Implemente uma função collatzLen x que retorna o tamanho da lista formada pela aplicação repetida de collatz sobre o valor x até que essa chegue no número 1.

-- Exercício 08: Encontre o número x entre 1 e 1.000.000 que tem a maior sequência de Collatz. (Project Euler 14)