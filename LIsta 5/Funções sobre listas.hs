--4. Dada uma lista de inteiros, elabore uma função que calcula o produto de todos os elementos pares da lista

quest4 :: [Int] -> Int
quest4 as = product [ x | x <- as , even x]

--5. Dado o símbolo ‘.‘ , construa uma string com 30 ‘.’.

quest5 :: Char -> String
quest5 as = replicate 30 '.'

--6. Defina uma função para verificar se uma palavra é palíndroma. Uma palavra é palíndroma se a ordem de leitura, da esquerda para direita ou da direita para a esquerda, é indiferente. Ex: arara.

quest6 :: String -> Bool
quest6 x = x == reverse x

--7. Dadas duas listas, uma do tipo String e outra do tipo Int, construa uma lista com pares do tipo (String, Int) de tal forma que cada elemento da primeira lista forme par com o correspondente elemento da segunda lista. Assim, por exemplo, para as listas [“Maria”, “Joana”, “Pedro”] e [18, 20, 17] a lista final deverá ser [(“Maria”, 18), (“Joana”, 20), (“Pedro”, 17)].

quest7 :: [String] -> [Int] -> [(String, Int)]
quest7 as ns = [ x |x <- zip as ns]

--Dada uma lista de (Estados, Capitais), construa um par com duas listas, onde o primeiro elemento é a lista de estados e o segundo elemento a de capitais.

quest8 :: [(String,String)] -> ([String],[String])
quest8 as = unzip as

--9. Dada uma lista de reais, elabore uma função para calcular a soma dos valores da lista maiores ou iguais a 5.0.

quest9 :: [Float] -> Float
quest9 as = sum [ x |x <- as , x >= 5.0] 

--10. Dada uma lista de números Float, elabore uma função para calcular a média dos valores da lista.

quest10 :: [Float] -> Float
quest10 as = sum [x | x <-as ] / fromIntegral (length  as) 

--11. Dada uma lista de notas de alunos, elabore uma função para determinar a lista das notas acima da média aritmética das notas da lista.

quest11 :: [Float] -> [Float]
quest11 as = [x | x <- as , quest10 as >= x ]

-- 12.Defina uma função para calcular o factorial de um número inteiro não negativo.

quest12 :: Int -> Int
quest12 a = product [x | x <-[a,a-1..1]]

-- 13. Escreva uma função que dada uma string, devolva a mesma string com o caractere ‘\n’ ao final. Ex: se a entrada for “gato” retornará “gato\n”.

quest13 :: String ->  String
quest13 a = a ++ "\n"

--14. Use a questão anterior, para construir a função que dada uma lista de strings devolve uma única string contendo a concatenação das strings da lista fornecida, tal que estas strings estejam separadas por \n. Ex: se a entrada for [“gato”, “e”, “rato”] retornará “gato\ne\nrato\n”

quest14 :: [String] -> String
quest14 as = concat [x ++ "\n" |x <- as ]

--15. Usando as duas últimas funções, faça uma função que imprime na tela uma lista de palavras de forma que cada palavra da lista apareça em uma linha da tela.

quest15 :: [String] -> IO ()
quest15 x = putStr (quest14 x)

--16. Faça uma função que, dada uma string e um inteiro representando um tamanho máximo de impressão para uma informação, devolva a mesma string justificada à direita, deixando espaços em branco na frente da string, se for o caso. Ex: se você fornecer “papagaio” e 15 de tamanho, a função deve retornar “ papagaio”, incluindo assim 7 espaços em branco à esquerda de papagaio que tem 8 caracteres, para completar os 15 caracteres de tamanho reservados.

quest16 :: String -> Int -> String
quest16 a x = replicate (x - length a) ' '  ++ a