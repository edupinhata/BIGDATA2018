{-
    Eduardo Pinhata
    22/Mar/2018
-}


-- Exercício 01: Crie uma função divisivel20 x que retorna verdadeiro se x for divisível por todos os números de 1 a 20.
divisivel20 :: Integer -> Bool
divisivel20 x = foldl (&&) True [ (rem x i) == 0 | i <- [1..20]]


-- Exercício 02: Crie uma função projectEuler5 que retorna o primeiro número natural que retorna True para a função do
-- exercício anterior. Pense em como reduzir o custo computacional.

-- verifica se x é divisível pelos primeiros n elementos 
-- de uma lista
divisiveln :: Integer -> Integer -> Bool
divisiveln x n = foldl (&&) True [ (rem x i) == 0 | i <- [1..n]]

-- verifica se há algum elemento da lista que pode ser 
-- divisível por x
(./?) :: [Integer] -> Integer -> Bool
(./?) l x = foldl (||) False [ (rem i x) == 0 | i <- l]

-- divide os elementos de uma lista por n, mas apenas
-- os que são divisiveis por n
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
projectEuler4 :: Integer -> Integer
projectEuler4 n = firstDiv 1
    where
        firstDiv x 
            | divisiveln x n == True = x
            | otherwise = firstDiv (x+1)


-- Método rápido
projectEuler5 :: Integer -> Integer
projectEuler5 n = mdc [1..n] 1 2 
        where 
            mdc l m primo 
                | ((sum l) == toInteger(length l)) || (primo > (sum l)) = m
                | ( l ./? primo == True) = mdc (l ./ primo) (m*primo) primo 
                | ( l ./? primo /= True) = mdc l m (nextPrimo primo) 


            
-- Exercício 03: Crie a lista de números de Fibonacci utilizando uma função geradora.
fibonacci :: Integer -> [Integer]
fibonacci 0 = [0]
fibonacci 1 = [0, 1]
fibonacci n = fibonacci' n [0, 1] 0 1
fibonacci' 0 l x y = l
fibonacci' n l x y = fibonacci' (n-1) (l++[x+y]) y (x+y)


-- Exercício 04: Utilizando a lista anterior, calcule a soma dos números de Fibonacci pares dos valores que não excedem 4.000.000. (Project Euler 2)
sumFibo :: [Integer] -> Integer
sumFibo l = foldl (+) 0 (takeWhile (<4000000) [i | i <- l, i `mod` 2 == 0])

-- Exercício 05: Faça uma função para calcular o produto escalar entre dois vetores.
(.*.) :: [Double] -> [Double] -> Double
(.*.) l1 l2 = foldl (+) 0 $ zipWith (*) l1 l2  

--Exercício 06: Crie a função collatz x que retorna x/2, se x for par e (3x+1) se for ímpar.
collatz :: Integer -> Integer
collatz x 
  | x `rem` 2 == 0 = x `div` 2
  | otherwise = (3*x)+1

-- Exercício 07: Implemente uma função collatzLen x que retorna o tamanho da lista formada pela 
-- aplicação repetida de collatz sobre o valor x até que essa chegue no número 1.
sizeCollatz :: Integer -> Int
sizeCollatz x = sizeCollatz' x []
    where
        sizeCollatz' 1 l = length l
        sizeCollatz' x l = sizeCollatz' (collatz x) ((collatz x):l)


-- Exercício 08: Encontre o número x entre 1 e 1.000.000 que tem a maior sequência de Collatz. (Project Euler 14)
biggestCollatz :: Integer
biggestCollatz = bc 1 0 0
    where
        bc 1000000 best nBest = best
        bc x best nBest 
            | (sizeCollatz x) > nBest = bc (x+1) x (sizeCollatz x)
            | otherwise = bc (x+1) best nBest