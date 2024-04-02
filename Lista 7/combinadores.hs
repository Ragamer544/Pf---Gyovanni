import Data.Char

main = do
  putStrLn "Hello, World!"



-- 1.Defina uma função para converter uma lista de números em uma lista dos quadrados destes números.

quadrados :: [Int] -> [Int]
quadrados xs = map quad xs
 where
 quad x = x^2 

--2.Defina uma função para dado uma lista de pares cartesianos retornar uma lista dos segundos elementos destes pares.

segundoElem :: [(Int,Int)] -> [Int]
segundoElem xs = map se xs
 where 
 se (a,b) = b

-- 3.Defina uma função para dado uma lista de pares cartesianos retornar uma lista dos segundos elementos destes pares.

lenght' :: [Int] -> Int
lenght' xs = sum ( map um xs )
 where
    um _ = 1

--4. Usando apenas a função zipWith, elabore:
-- ○ Função para retornar uma lista contendo o dobro dos elementos de uma lista de inteiros.
-- ○ Função para retornar uma lista contendo os quadrados dos elementos de uma lista de inteiros.

dobraLista :: [Int] -> [Int]
dobraLista xs = zipWith (+) xs xs

returnDobro :: [Int] -> [Int]
returnDobro xs = zipWith (*) xs xs

--5. Defina uma função para verificar se uma lista está ordenada. Dica: use zipWith e and.

verificaOrd :: [Int] -> Bool
verificaOrd [x] = True
verificaOrd xs = and (zipWith (<=) xs (tail xs) )

--6. Elabore uma função para verificar se dado uma string ela não  contém dígitos.

verificaDig :: String -> Bool
verificaDig xs = null (filter isDigit xs)

--7. Elabore uma função para verificar se dado uma string ela contém somente caracteres alpha (letras).

verficaAlpha :: String -> Bool
verficaAlpha xs = null (filter (not.isAlpha) xs)

-- 8. Elabore uma função para verificar se dado uma string ela contém algum caractere que não seja alpha.

verificanotAlpha :: String -> Bool
verificanotAlpha xs = ((filter isAlpha xs) == xs)

--9. Defina uma função para selecionar os elementos positivos de uma lista de números.

verificaNegativo :: [Int] -> [Int]
verificaNegativo xs = filter maior xs
 where
    maior x = x>0

-- 10. Defina uma função para selecionar pares na forma (a, a^2), de uma lista de pares.

devolvePar :: Num a => [(a,a)] -> [(a,a)]
devolvePar xs = map se xs
 where
    se (a,b) = (a,b^2)

--11. Defina uma função que, dada uma lista xs e um valor v, retorne todas as posições em que v ocorre em xs.

retornaPosicao :: [Int] -> Int -> [Int]
retornaPosicao xs v = map snd (filter iguais dupla)
 where
    dupla = zip xs [0..]
    iguais (a,b) = a == v

--12.Defina uma função para dada uma lista e um valor x determinar a última posição de x na lista, ou -1 se ele não ocorrer na lista.
retornaUlt :: [Int] -> Int -> Int
retornaUlt xs v = if last (map snd (filter iguais1 dupla1)) /= [] then last (map snd (filter iguais1 dupla1)) else -1
 where
    dupla1 = zip xs [0..]
    iguais1 (a,b) = a == v

--13. Dada a função addUp ns = filter greaterOne (map succ ns) where greaterOne n = n > 1
-- como pode redefini-la usando filter antes do map, assim
-- addUp ns = map fun1 (filter fun2 ns)

addUp ns = filter (>1) (map succ ns)

addUp' ns = map succ (filter (>0) ns)

--16.Usando somente as funções vistas até o momento, defina funções para:

-- ○ Retornar o menor valor da função f nas entradas 0 até n.

menorValor :: (Int -> Int) -> Int -> Int
menorValor f n = minimum (map f [0..n])

-- ○ Verificar se os valores de f nas entradas 0 até n são todos iguais.
valorIgual :: (Int -> Int) -> Int -> Bool
valorIgual f n = all (filter (f n == f x )[0..n])

-- ○ Verificar se os valores de f nas entradas 0 até n são todos maiores que zero.

maiorZero :: (Int -> Int) -> Int -> Bool
maiorZero f n = all (filter (f x > 0) x)
 where
 x = [0..n]
-- ○ Verificar se os valores f 0, f 1 até f n estão em ordem crescente.

estaoEmOrdemCrescente :: (Int -> Int) -> Int -> Bool
estaoEmOrdemCrescente f n = all (filter (f a <= f b) (a,b))
 where
    (a,b) = zip [0..n] [1..n] 