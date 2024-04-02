-- 2. Dado n e ns, calcular quantas ocorrências de n há em ns.

contaQuantidade :: Int -> [Int] -> Int
contaQuantidade n [] = 0
contaQuantidade x (n:ns)
    | n == x    = 1 + contaQuantidade x ns
    | otherwise = contaQuantidade x ns

--3. Dadas ns e ms, verificar se ambas listas são disjuntas (verificar se não têm elementos comuns).

verificaElementos :: [Int] -> [Int] -> Bool
verificaElementos [] _ = True
verificaElementos (n:ns) ms
 | n `elem` ms = False
 | otherwise = verificaElementos ns ms

--4. Dado n e ns, eliminar a última ocorrência de n em ns. Caso n não ocorra em ns, o resultado será ns.

eliminaUltimo :: Int -> [Int] -> [Int]
eliminaUltimo _ [] = []
eliminaUltimo x (n:ns) = reverse  (eliminaPaz x(reverse (n:ns)))
 where
    eliminaPaz x (n:ns)
     | x == n = ns
     | otherwise = n : eliminaPaz x ns 

--5.Dadas duas listas ns e ms, tais que nem ns e nem ms contêm elementos repetidos, realizar a união de ambas listas sem que o resultado tenha elementos repetidos. Observe que podem haver elementos comuns que estejam em ns e em ms.

uniaoSemRepeticao :: [Int] -> [Int] -> [Int]
uniaoSemRepeticao [] ms = ms
uniaoSemRepeticao (n:ns) ms
    | elem n ms = uniaoSemRepeticao ns ms
    | otherwise = n : uniaoSemRepeticao ns ms

--6.Dado n e ns, retornar a posição (ou índice) em que n ocorre dentro de ns. Se n ocorrer
-- várias vezes, retornar a primeira posição. Se n não ocorrer em ns, retornar -1. As
-- posições são contadas a partir de zero.

encontrarPosicao :: Int -> [Int] -> Int
encontrarPosicao _ [] = -1
encontrarPosicao n ns = encontrarP n ns 0
    where
        encontrarP _ [] _ = -1
        encontrarP x (y:ys) z
            | x == y    = z
            | otherwise = encontrarP x ys (z + 1)

--7. Dado n e ns, retornar a lista com todas as posições em que n ocorre dentro de ns.

posicoesOcorrencias :: Int -> [Int]
posicoesOcorrencias _ [] = []
posicoesOcorrencias n ns = encontrarP n ns 0
    where
        encontrarP _ [] _ = []
        encontrarP x (y:ys) z
            | x == y    = z : encontrarP x ys (z + 1)
            | otherwise = encontrarP x ys (z + 1)