import Data.Char 
main = return ()

-- 1. O produto dos elementos de uma lista de inteiros.

produtt :: [Int] -> Int
produtt [] = 1
produtt (x:xs) = x * produtt xs

--2. Filtrar os números ímpares, ou seja, da lista original, ficar somente com os ímpares

filtrarImpar :: [Int] -> [Int]
filtrarImpar (x:xs) 
 | odd x = x : filtrarImpar xs
 |otherwise = filtrarImpar xs 

 --3. Triplicar todos os elementos de uma lista de inteiros.

tripleInt :: [Int] -> [Int]
tripleInt [] = []
tripleInt (x:xs) = (x*3) : tripleInt xs

--4. Triplicar somente os pares, manter os outros intactos.

tripleEven :: [Int] -> [Int]
tripleEven [] = []
tripleEven (x:xs)
 | even x = (x*3) : tripleEven xs
 | otherwise = x :tripleEven xs

 --5.Triplicar os pares, eliminar os ímpares.

triplica :: [Int] -> [Int]
triplica [] = []
triplica (x:xs) 
 | even x = 3 * x : triplica xs
 |otherwise = triplica xs 

--  6. Verificar se uma string contém algum caractere alfabético (letra, acentuada ou não).Use a funçãoisAlpha :: Char -> Bool , da biblioteca Data.Char.

funçãoisAlpha :: Char -> Bool
funcaoisAlpha [] = True
funcaoisAlpha (x:xs) = isAlpha x && isAlpha xs 

-- 7. Eliminar a primeira ocorrência de um dado elemento, se ele ocorrer, senão retornar a lista original.

eliminaTermo :: [Int] -> [Int] -> [Int]
eliminaTermo [] = []
eliminaTermo x (y:ys)
 | x == y = ys
 | otherwise = (y : eliminaTermo x ys)

 -- 8. Eliminar todas as ocorrências de um dado elemento.

eliminaTudo :: [Int] -> [Int] -> [Int]
eliminaTudo [] = []
eliminaTudo x (y:ys)
 | x == y = eliminaTudo x ys
 | otherwise =  (y : eliminaTudo x ys)  

 --9. Inverter uma lista de inteiros.

inverteLista :: [Int] -> [Int]
inverteLista [] = []
inverterLista xs = last xs : inverteLista (init xs)

-- 10. Dadas duas listas ns e ms, retornar a lista de todos os elementos que estão em ns e ms ao mesmo tempo. Elementos repetidos de ns se mantém repetidos na lista resultante.

intersepta :: [Int] -> [Int] -> [Int]
intersepta [] _ = []
intersepta (n : ns) ms 
 | elem n ms = n : intersepta ns ms
 | otherwise = intersepta ns ms 
 
 -- 11. Dadas duas listas ns e ms, retornar a lista de todos os elementos que estão em ns e não estão em ms. Elementos repetidos de ns se mantêm repetidos.

naoConsta :: [Int] -> [Int] -> [Int]
naoCosta [] _ = []
naoConsta (n : ns) ms 
 | notElem n ms = n : naoConsta ns ms
 | otherwise = naoConsta ns ms 


--12.Dada uma lista ns retornar a lista que contenha todos os elementos de ns, porém sem nenhuma repetição (sem nenhum elemento repetido).

semRepetição:: [Int] -> [Int]
semRepetição [] _ = []
semRepetição (n : ns) 
 | elem n ms = n : semRepetição ns 
 | otherwise = semRepetição ns 