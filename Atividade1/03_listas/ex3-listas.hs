{-
    Eduardo Pinhata
    22/Mar/2018
-}


-- Exercício 01: Crie uma função divisivel20 x que retorna verdadeiro se x for divisível por todos os números de 1 a 20.
divisivel20 :: Integer -> Bool
divisivel20 x = foldl (&&) True [ (rem x i) == 0 | i <- [1..20]]


-- Exercício 02: Crie uma função projectEuler5 que retorna o primeiro número natural que retorna True para a função do
-- exercício anterior. Pense em como reduzir o custo computacional.
divisivel ::  Integer -> [Integer]  -> Bool
divisivel x l = foldl (||) False [ (rem i x) == 0 | i <- l ]

divisiveln :: Integer -> Integer -> Bool
divisiveln x n = foldl (&&) True [ (rem x i) == 0 | i <- [1..n]]

(./?) :: [Integer] -> Integer -> Bool
(./?) l x = foldl (||) False [ (rem i x) == 0 | i <- l]

-- mmc
-- mdc
-- fazendo os dois eu consigo char o primeiro número divisível por todos os números de 1 a n
(./) :: [Integer] -> Integer -> [Integer]
(./) l x = saoDiv ++ naoSaoDiv
    where
        saoDiv = [ (div i x) | i <- l, (rem i x == 0 )] 
        naoSaoDiv = [i | i <- l, (rem i x /= 0) ]

isPrimo :: Integer -> Bool
isPrimo x = not $ foldl (||) False [ (rem x i) == 0 | i <- [2..(x-1)] ]


nextPrimo :: Integer -> Integer
nextPrimo x = isPrimo' (x+1) 
    where
        isPrimo' x
            | isPrimo x == True = x
            | otherwise         = isPrimo' (x+1) 
   

-- Força Bruta
projectEuler5 :: Integer -> Integer
projectEuler5 n = firstDiv 1
    where
        firstDiv x 
            | divisiveln x n == True = x
            | otherwise = firstDiv (x+1)


projectEuler6 :: Integer -> Integer
projectEuler6 n = mdc [1..n] 1 2 
        where 
            mdc l m primo 
                | ((sum l) == toInteger(length l)) || (primo > (sum l)) = m
                | ( l ./? primo == True) = mdc (l ./ primo) (m*primo) primo 
                | ( l ./? primo /= True) = mdc l m (nextPrimo primo) 


            
-- Exercício 03: Crie a lista de números de Fibonacci utilizando uma função geradora.

-- Exercício 04: Utilizando a lista anterior, calcule a soma dos números de Fibonacci pares dos valores que não excedem 4.000.000. (Project Euler 2)

-- Exercício 05: Faça uma função para calcular o produto escalar entre dois vetores.

--Exercício 06: Crie a função collatz x que retorna x/2, se x for par e (3x+1) se for ímpar.

-- Exercício 07: Implemente uma função collatzLen x que retorna o tamanho da lista formada pela aplicação repetida de collatz sobre o valor x até que essa chegue no número 1.

-- Exercício 08: Encontre o número x entre 1 e 1.000.000 que tem a maior sequência de Collatz. (Project Euler 14)