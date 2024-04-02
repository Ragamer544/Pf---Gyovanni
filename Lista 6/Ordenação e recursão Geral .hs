import Data.Char

--1. Defina a função
--merge :: [Int] -> [Int] -> [Int]
--de tal maneira que ao receber duas listas ordenadas xs e ys, merge xs ys
--retorna todos os elementos de xs e ys numa única lista, também ordenada.

merge :: [Int] -> [Int] -> [Int]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) 
 | x <= y = x : merge xs (y:ys)
 | otherwise = y : merge (x:xs) ys 

--2. Usando merge, defina a função
--mergeSort :: [Int] -> [Int]
--que ordena a lista de entrada. A ideia é dividir a lista de entrada em duas partes de
--tamanho igual ou no máximo variando em um, recursivamente ordenar cada parte e
--então aplicar merge.

mergeSort :: [Int] -> [Int]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = merge(mergeSort(fst duplas)) (mergeSort (snd duplas))
 where
    duplas = splitAt (div n 2) xs
    n = length xs

--3. Dadas duas listas ordenadas ascendentemente, cada uma sem elementos
-- repetidos, defina funções para calcular a união e a interseção. Os resultados devem
-- ser listas ordenadas sem elementos repetidos. Não pode usar nenhuma função
-- pré-definida. Em particular, não pode usar elem e nem definir uma função que faça
-- o mesmo que ela.3. Dadas duas listas ordenadas ascendentemente, cada uma sem elementos
-- repetidos, defina funções para calcular a união e a interseção. Os resultados devem
-- ser listas ordenadas sem elementos repetidos. Não pode usar nenhuma função
-- pré-definida. Em particular, não pode usar elem e nem definir uma função que faça
-- o mesmo que ela.

fElem :: Int -> [Int] -> [Int]
fElem y [] = []
fElem y (x:xs)
  | y == x = x : fElem y xs
  | otherwise = fElem y xs 

intersecao :: [Int] -> [Int] -> [Int]
intersecao [] _ = []
intersecao _ [] = []
intersecao(n:ns) (m:ms)
 | n < m     = intersecao ns (m:ms)
 | n > m     = intersecao (n:ns) ms
 | otherwise = n : intersecao ns ms

uniao :: [Int] -> [Int] -> [Int]
uniao [] ys = ys
uniao xs [] = xs
uniao (x:xs) (y:ys) 
 | x < y  &&  x /= y = x : uniao xs (y:ys)
 | otherwise = y : uniao ys xs 


--4. Utilizando somente as funções chr e ord do módulo Data.Char, definir uma
--função que transforme um número natural para string.

transfNumero:: Int -> String
transfNumero x
 | x >= 0 && x <= 9 = [paraChar x]
 | otherwise = transfNumero (div x 10) ++ [paraChar (mod x 10)]
  where
   paraChar :: Int -> Char
   paraChar n = chr (ord '0' + n)

-- 5. Defina uma função que calcule a soma de todos os dígitos de um número. Por
-- exemplo, para o número 2315 a função deverá retornar 2+3+1+5, ou seja 11.

somaDigitos :: Int -> Int
somaDigitos 0 = 0
somaDigitos n = (n `mod` 10) + somaDigitos (n `div` 10)