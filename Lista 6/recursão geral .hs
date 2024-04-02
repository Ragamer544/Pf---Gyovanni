--1.Sem usar mod, defina uma função meuMod que faça o mesmo que mod.

meuMod :: Int -> Int -> Int
meuMod x 0 = error "não é divisivel por zero"
meuMod x y 
 | x < y = x
 |otherwise = meuMod (x-y) y

meuDiv :: Int -> Int -> Int
meuDiv x 0 = error "não é divisivel por zero"
meuDiv 0 y = 0
meuDiv x y 
 | x < y = 0
 | otherwise = 1 + meuDiv (x-y) y

--2.Sem usar nem div nem mod, defina uma função chamada divMod que calcula ao
--mesmo tempo a divisão inteira e o resto da divisão inteira (retorna uma dupla).

divMod' :: Int -> Int -> (Int,Int)
divMod' x 0 =  error "não é divisivel por zero"
divMod' 0 y = (0, 0)
divMod' x y
 | x < y = (0,x)
 | otherwise = (1 + q, r)
  where
   (q,r) = divMod' (x-y) y


--3. Outra propriedade do mdc (máximo divisor comum) é que se a > b, então mdc a b = mdc (a `mod` b) b
--Usar esta propriedade para dar uma definição --recursiva para a função mdc.

meuMdc :: Int -> Int -> Int
meuMdc x 0 = x
meuMdc 0 y = y
meuMdc x y
 | x >= y = meuMdc (meuMod x y) y
 |otherwise = meuMdc x (y-x)

-- 4.Considere a definição de myTake vista em aula. O que acontece quando o primeiro
-- argumento é negativo? Redefina myTake de tal forma que myTake n xs retorne []
-- quando n < 0.

myTake :: Int -> [Int] -> [Int]
myTake x [] = []
myTake x ys
 | x <= 0 =[]
 |otherwise = head ys : myTake (x-1) (tail ys)

-- 5.Defina a função myDrop que faz o mesmo que a função do Prelude drop. Tenha cuidado para que myDrop n xs retorne xs quando n < 0.

myDrop :: Int -> [Int] -> [Int]
myDrop x [] = []
myDrop x ys
 | x <= 0 = ys
 | otherwise = myDrop (x-1) (tail ys)

--6. Defina a função myLast que retorna o último elemento de uma lista.

myLast :: [Int] -> Int
myLast [x] = x
myLast (x:xs) = myLast xs 

--7.Defina a função myInit que retorna todos os elementos de uma lista menos o último.

myInit :: [Int] -> [Int]
myInit [x] = []
myInit (x:xs) = x : myInit xs

--8.Sem usar ranges, defina a função enumereDeAte :: Int -> Int -> [Int] tal que enumereDeAte m n seja igual com o range [m..n].

enumereDeAte :: Int -> Int -> [Int]
enumereDeAte x 0 = []
enumereDeAte x y
 | x > y = []
 | otherwise = x : enumereDeAte (x+1) y

 --9.Sem usar ranges, defina a função enumereDeEntaoAte :: Int -> Int -> Int -> [Int] tal que enumereDeEntaoAte m n p seja igual com o range [m,n..p].

enumereDeEntaoAte :: Int -> Int -> Int -> [Int]
enumereDeEntaoAte m y 0 = []
enumereDeEntaoAte m n p
 | m > p = [] 
 | m == n && n == p = enumereDeEntaoAte m n p 
 | otherwise = x : enumereDeEntaoAte (m+(n-m)) n p


--10. Defina a função filtraPosicoesPares :: [Int] -> [Int] que retorna todos os elementos da lista de entrada que estão em posições pares.

filtraPosicoesPares :: [Int] -> [Int] 
filtraPosicoesPares [] = []
filtraPosicoesPares [x] = []
filtraPosicoesPares (x:xs) = x : filtraPosicoesPares xs
 

--11.Defina a função filtraPosicoesImpares :: [Int] -> [Int] que retorna todos os elementos da lista de entrada que estão em posições ímpares.

filtraPosicoesImpares :: [Int] -> [Int]
filtraPosicoesImpares [] = []
filtraPosicoesImpares [x] = [x]
filtraPosicoesImpares (y:ys) = filtraPosicoesImpares ys
 

-- 12. Definir as funções filtraPosicoesPares :: [Int] -> [Int] filtraPosicoesImpares :: [Int] -> [Int] usando recursão mútua.

filtraPosicoesPares1 :: [Int] -> [Int]
filtraPosicoesPares1 [] = []
filtraPosicoesPares1 (n:ns) = n : filtraPosicoesImpares1 ns


filtraPosicoesImpares1 :: [Int] -> [Int]
filtraPosicoesImpares1 [] = []
filtraPosicoesImpares1 (m:ms) = filtraPosicoesPares1 ms

-- --13. Considere a seguinte série
-- 0 1 2 2 3 5 7 10 15 22 32 ...
-- Defina uma função que, dado n, retorne o n-ésimo elemento da série

serie :: Int -> Int
serie 0 = 0
serie 1 = 1
serie 2 = 1 
serie n = serie (n-1) + serie (n-3)

-- 14. Defina a função
-- myZip :: [Int] -> [Int] -> [(Int, Int)]
-- que recebe duas listas xs e ys e retorna uma lista de duplas. Na lista resultante, a
-- primeira dupla é formada com ambos os primeiros elementos de xs e ys, a segunda
-- dupla com os segundos elementos de xs e ys e assim por diante. Se uma das listas xs
-- ou ys tem mais elementos que a outra, os elementos que sobram são ignorados.
-- Assim, por exemplo
-- myZip[2,3,4] ['a', 'b', 'c', 'd'] =
-- [(2, 'a'), (3, 'b'),(4,'c')]

myZip :: [Int] -> [Int] -> [(Int, Int)]
myZip [] ys = []
myZip xs [] = []
myZip (x:xs) (y:ys) = ((x,y): myZip xs ys)

--15. Defina a função
-- myUnzip :: [(Int, Int)] -> ([Int], [Int])
-- que faz o inverso de myZip. Assim, por exemplo
-- myUnzip [(2, 'a'), (3, 'b'),(4,'d')] =
-- ([2,3,4], ['a', 'b', 'c'])

myUnzip :: [(Int, Int)] -> ([Int], [Int])
myUnzip [] = ([],[])
myUnzip ((a,b):ds) = (a : as , b : bs)
 where
   (as,bs) = myUnzip ds