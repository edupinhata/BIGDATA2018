{-|
Introdução a Haskell
-}

--Exercício 01: Crie uma função ehTriangulo que determina se três lados x, y, z podem formar um triângulo.
ehTriangulo :: Double -> Double -> Double -> Bool
ehTriangulo a b c = c1 && c2 && c3
    where 
        c1 = side a b c
        c2 = side b a c
        c3 = side c a b
        side x y z = (abs (y-z) < x) && (x < (y+z))

--Exercício 02: Crie uma função tipoTriangulo que determina o tipo do triângulo formado pelos três lados x, y, z.
tipoTriangulo :: Double -> Double -> Double -> [Char]
tipoTriangulo x y z 
    | ehTriangulo x y z == False = "Nao eh triangulo."
    | x == y && y == z  = "Equilatero" 
    | isoceles          = "Isoceles"
    | otherwise         = "Escaleno"
    where
        isoceles = (x==y) || (x==z) || (z==y)

--Exercício 03: Implemente uma função que faz a multiplicação etíope entre dois números.
etiope ::  Int -> Int -> Int
etiope y x = etiope' 0 y x
    where
        etiope' n 1 x = n+x
        etiope' n y x 
            | rem y 2 == 0 = etiope' n (div y 2) (x*2)
            | rem y 2 == 1 = etiope' (n+x) (div y 2) (x*2)

-- Versão sem Guards
etiope2 :: Int -> Int -> Int
etiope2 y x = etiope2' 0 y x
        where   
            etiope2' n 1 x = (n+x)
            etiope2' n y x = etiope2' (getN n y x) (div y 2) (x*2)
            getN n y x = n + (x*(rem y 2))

--Exercício 04: Faça uma função que determine se um número é primo.
ehPrimo :: Int -> Bool
ehPrimo n = ehPrimo' n (n-1)
ehPrimo' n 1 = True
ehPrimo' n m
  | mod n m /= 0 = ehPrimo' n (m-1)
  | mod n m == 0 = False 

--Exercício 05: Faça uma função que calcule a soma dos dígitos de um número.
lastDigit ::  Int -> Int
lastDigit x = fromIntegral $ round $ ((fromIntegral x / 10) - fromIntegral(div x 10 ))*10

sumDigit :: Int -> Int
sumDigit n = sumDig' n 0
sumDig' 0 m = m
sumDig' n m = sumDig' (div n 10) (m + lastDigit n)


--Exercício 06: Faça uma função que calcule a persistência aditiva de um número.
additivePers :: Int -> Int
additivePers n
  | div n 10 == 0 = n
  | otherwise = additivePers $ sumDigit n


--Exercício 07: Faça uma função que calcule o coeficiente binomial de (m,n)
factorial :: Int -> Int
factorial n = factorial' n 1
factorial' 1 m = m
factorial' n m = factorial' (n-1) (m*n)
binomial :: Int -> Int -> Int
binomial n k = n1 `div` n2 
    where 
        n1 = factorial n
        n2 = (factorial k) * (factorial(n-k))

{- A divisão não está funcionando
binomial :: Int -> Int -> Int
binomial n k = (n1 n) / (n2 n k)
  where
    n1 x = fromIntegral $ factorial x 
    n2 x y = fromIntegral $ (factorial y) * (factorial (x-y)) 
-}

-- outra forma
choose :: Int -> Int -> Int
choose n k = (product [1..n]) `div` (product [1..k] * product [1..(n-k)])


--Exercício 08: Faça uma função que calcule o elemento (i,j) do triângulo de pascal.
-- por recursão
triPasc :: Int -> Int -> Int
triPasc _ 0 = 1
triPasc 0 _ = 1
triPasc i j = triPasc (i-1) j + triPasc i (j-1)

triPasc2 :: Int -> Int -> Int
triPasc2 m n = n1 `div` n2
    where 
        n1 = product [m+1..(m+n)]
        n2 = product [1..n]