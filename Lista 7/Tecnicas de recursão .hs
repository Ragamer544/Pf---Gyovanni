--1. Dado n, imprimir a tabuada de multiplicação do n.
imprimirTabuadaAux :: Int -> Int -> IO ()
imprimirTabuadaAux n i
  | i > 10 = return ()
  | otherwise = do
      putStrLn (show n ++ " x " ++ show i ++ " = " ++ show (n * i))
      imprimirTabuadaAux n (i + 1)


--2. Dada uma lista de números Double, calcular quantos estão acima da média.

acimaMedia :: [Double] -> Int
acimaMedia [] = 0
acimaMedia (x:xs) 
 |  x > media = 1 + acimaMedia xs
 | otherwise = acimaMedia xs
  where
    media = sum (x:xs)/ fromIntegral (length (x:xs))

--3. Dada uma lista de pares (Nota, NomeAluno), calcular os nomes daqueles que têm nota acima da média.

type NomeAluno = String
type Nota = Double

nomeAcima :: [(Nota,NomeAluno)] -> [NomeAluno]
nomeAcima [] = []
nomeAcima ((a,b):xs)
 | a > media = b : nomeAcima xs
 | otherwise = nomeAcima xs
  where
     media = somaA/ fromIntegral (length ((a,b):xs))
     somaA = sum(fst(unzip((a,b):xs)))



--4. Para este exercício utilize o CodeWorld. Dados n e t, construir um polígono regular (uma Picture) de n lados com tamanho de lado t.


--5. Resolva o problema de encontrar o máximo divisor comum de dois inteiros positivos a e b generalizando o problema para outro que, dentre os números que estão no intervalo entre 1 e m, encontre o maior divisor comum de a e b.

mdc' :: Int -> Int -> Int
mdc' a b = mdcAux (min a b)
 where
   mdcAux m 
    | mod a m == 0 && mod b m == 0 = m
    | otherwise = mdcAux (m-1)

--6.Resolva o problema de encontrar o mínimo múltiplo comum (mmc) de dois inteiros positivos a e b generalizando o problema para outro que, dentre os números que estão no intervalo entre m e n, encontre o menor múltiplo comum entre a e b.

mmc :: Int -> Int -> Int
mmc a b = mmc' (max a b)
  where
   mmc' m
    | mod m (min a b) == 0 = m
    | otherwise = mmc' (m + (max a b))

--8.Considere a série :0 1 2 2 3 5 7 10 15 22 32. Dê uma definição eficiente para calcular o n-ésimo número da série. A função recebe como argumento o valor de n.

serie :: Int -> Int
serie n = serieAux n 0 1 2 2 
 where
 serieAux 0 a _ _ _ = a
 serieAux 1 _ b _ _ = b
 serieAux 2 _ b _ _ = b
 serieAux 3 _ _ c _ = c
 serieAux n a b c d = serieAux (n - 1)  b  c  d (a+b+c)

  
--9.Defina uma função que avalie a série i−0 n ∑ 1 i!

eAp :: Double -> Double 
eAp m = fst (eAp' m)
 where
 eAp' 0 = (1,1)
 eAp' n = (s+t/n, t/n)
   where
   (s,t) = eAp' (n-1)

--10. Uma subsequência de um string st é uma sequência que está contida dentro de st, com todos seus elementos contíguos. Dada uma String st, calcular o tamanho da maior subsequência de vocais. Como informação extra, peça ao amigo o tamanho do maior prefixo de st formado só por vogais.


sv :: String -> (Int,Int) 
sv "" = (0,0)
sv (c:cs) 
 | eVogal c && p == s = (p+1,p+1)
 | eVogal c = (s,p+1)
 | otherwise = (s,0)
  where
   (s,p) = sv cs  
   
eVogal :: Char -> Bool
eVogal c = elem c "aeiouAEIOU"


--11.Estender a solução do exercício anterior para calcular a subsequência (não o tamanho).

sv' :: String -> (Int, Int, String, String)
sv' "" = (0, 0, "", "")
sv' (c:cs)
  | eVogal c && p == s = (p+1, p+1, c : sec , c : sec )
  | eVogal c = (s, p+1, sec, c : sp)
  | otherwise = (s, 0, sec , "")
  where
    (s, p, sec,sp) = sv' cs

pegaSeq :: String -> String
pegaSeq xs = palavra
 where 
 (_,_, palavra,_)= sv' xs

  
--12. Calcular o fatorial de um natural

fatCauda :: Int -> Int 
fatCauda n = fatTc n 1

fatTc :: Int -> Int -> Int 
fatTc 0 acum = acum
fatTc n acum = fatTc (n-1) (acum * n)


--13. Calcular m^n .

pot :: Int -> Int -> Int
pot m n = potAux m n 1
 where
 potAux m 0 res = res 
 potAux m n res = potAux m (n-1) (res*m)


--14. Dada uma lista ns e um inteiro n, calcular a primeira posição de n em ns. Se n não ocorre dentro de ns, a função deve retornar -1.

retornaPosições :: Int -> [Int]-> Int
retornaPosições n ns = retornaPosiçõesAux n ns 0
 where
  retornaPosiçõesAux _ [] _ = -1
  retornaPosiçõesAux t (m:ms) res 
   | t == m = res
   | otherwise = retornaPosiçõesAux t ms (res+1)


--15. Dada uma lista ns e um inteiro n, calcular as posições de n em ns.

retornaTodasPosicoes :: Int -> [Int] -> [Int]
retornaTodasPosicoes n ns = retornaTodasPosicoesAux n ns 0
 where
  retornaTodasPosicoesAux _ [] _ = []
  retornaTodasPosicoesAux s (n:ns) res
   | s == n = res : retornaTodasPosicoesAux s ns (res+1)
   | otherwise = retornaTodasPosicoesAux s ns (res + 1)
  
--16. Defina uma função para calcular m^n .

pot' :: Int -> Int -> Int 
pot' m 0 = 1
pot' m n 
 | even n = p*p
 | otherwise = p * p * m
  where
   p = pot' m (div n 2)