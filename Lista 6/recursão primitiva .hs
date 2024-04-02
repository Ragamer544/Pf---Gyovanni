--1. Defina a função multiplica que multiplica dois números. Não pode usar o operador (*), mas pode usar (+).

multi :: Int -> Int -> Int
multi 0 m = 0
multi n m  = n + (multi (m-1))n

--2.Escreva uma função que receba um inteiro m e um natural n e calcule m . Não pode usar os operadores (**) e (^).

potencia :: Int -> Int -> Int
potencia 0 m = 0
potencia n m = m * (potencia (n-1))m

--
f :: Int -> Int
f n
 | n < 1000 = 2*n
 | otherwise = 5000 - n

retonarMaior :: Int -> Int
retonarMaior 0 = f 0
retornarMaior n 
 | f n  >= retonarMaior (n-1) = f n 
 | otherwise = retonarMaior (n -1) 

-- 3.Escreva uma função que calcule 1^3+ 2^3+ 3^3 +... + n^3

potn :: Int -> Int 
potn 0 = 0
potn n = n^3 + (potn (n - 1) )

--4. Escreva uma função que calcule : 2^0+ 2^1 + 2^2......2^n

pot :: Int ->  Int
pot 0 = 1 
pot n =  2^n + (pot (n-1))

--Escreva uma função calcule: 1/0! + 1/1! + 1/2! +... + 1/n!. Só como informação, esta série permite calcular aproximações para a constante de euler e. Em particular, o limite desta série é e.

euler :: Int -> Float
euler 0 = 1
euler n = (1 / fromIntegral (fat n))  + (euler n - 1)
 where
  fat :: Int -> Int
  fat 0 = 1
  fat n = n * (fat n - 1 )

-- 6. Escreva uma função que, dado um natural n, calcule a raiz quadrada inteira de n, ou seja, calcule o maior natural cujo quadrado é menor ou igual a n.

calculaRaiz :: Int -> Int
calculaRaiz n = raiz n 0
 where
  raiz :: Int -> Int -> Int
  raiz x y 
   | y * y > x = y - 1
   |otherwise = raiz x (y+1) 

-- 7. Defina uma função que aceite um natural n e devolva o maior valor dentre f 0, f 1,f 2, ... f n.

f :: Int -> Int
f n
 | n < 1000 = 2*n
 | otherwise = 5000 - n

retonarMaior :: Int -> Int
retonarMaior 0 = f 0
retornarMaior n 
 |f n  >= e = f n 
 |otherwise = e
 where
  e = retonarMaior (n - 1)


-- 8. Defina uma função que aceite um natural n e devolva True se e somente se um ou mais valores dentre f 0, f 1, f 2, ... f n é zero.

checarZero :: Int -> Bool
checarZero 0 = f 0 == 0 
checarZero n =  f n == 0 || checarZero ( n-1 )  