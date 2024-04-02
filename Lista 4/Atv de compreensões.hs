import Data.Char
main = return ()

--1) Defina uma função que dada uma lista de inteiros retorna os ímpares desta lista, porém triplicados.

funcaoTriplica :: [Int] -> [Int]
funcaoTriplica ns = [x*3 | x <- ns , odd x]

--2. Defina uma função que dada uma lista de inteiros retorna uma lista com os ímpares triplicados e os pares inalterados.

triplica :: [Int] -> [Int]
triplica ts = [if even x then x else 3*x | x <- ts]

--Defina uma função que permita verificar se um dado inteiro está dentro de uma lista.(Não pode usar elem).

funcaoVerificar :: Int -> [Int] -> Bool
funcaoVerificar x xs = [comp|comp <- xs,comp == x] /= []

--Defina uma função que dado um string e um inteiro n, construa a lista com este string repetido n vezes.

funcaoRepetir :: String -> Int -> [String]
funcaoRepetir a b = [a|_ <- [1..b]]

--Defina uma função que dada uma string, devolve a string em que todo caractere é transformado para maiúsculo. Utilize a função toUpper do módulo Data.Char.

funcaoTransformar :: String -> String
funcaoTransformar xs = [ toUpper x | x <- xs]

--6. Defina uma função que dada uma string, retorna a string eliminando todos os caracteres que são dígitos. Utilize a função isDigit do módulo Data.Char.

funcaoElimina :: String -> String
funcaoElimina xs = [ x|x <-xs, not (isDigit x) ]

-- 7. Defina uma função para verificar se algum elemento de uma lista dada é par.

funcaoPar :: [Int] -> Bool
funcaoPar xs = [x| x <- xs,even x] /= []

--8. Defina uma função que permita calcular o máximo divisor comum de dois números. Se precisar, pode usar as funções max e min que calculam o maior e o menor dentre dois números, respectivamente. Também pode precisar usar a função head que retorna o primeiro elemento de uma lista.

funcaoMDC :: Int -> Int -> Int
funcaoMDC a b = last [x| x <-[1..(max a b)], mod a x == 0 && mod b x == 0]

--Defina uma função que, dadas duas listas de palavras, sendo uma de adjetivos e outra de nomes, construa todas as combinações possíveis de nomes com adjetivos. Assim, por exemplo, se temos adjetivos = ["bonito", "alegre", "inquieto"] nomes = ["menino", "garoto"] a função aplicada a adjetivos e nomes deve retornar [ "menino bonito", "menino alegre", "menino inquieto", "garoto bonito", "garoto alegre", "garoto inquieto"

funcaoNome :: [String] -> [String] -> [String]
funcaoNome xs ns = [x ++ "_" ++ y|x <- xs , y <- ns]

--10. Defina uma função que dada uma lista de strings retorna a string obtida pela concatenação de todos os strings na lista. Dica: utilize compreensão com dois geradores.

funcaoConcatena :: [String]  -> String
funcaoConcatena xs  = [n|x <- xs , n <- x]

--Defina uma função que receba uma lista de strings e que retorne um string que quando impresso na tela com putStr mostre cada string da lista em uma linha separada.

pulaLinha :: [String] -> String
pulaLinha xs = funcaoConcatena [x ++ "\n" |x <- xs]

--Defina uma função que dado um número n, produza a tabela de multiplicação do n. Assim, por exemplo, se n=3, a função produzirá um string que, ao apresentar na tela, seguirá este padrão. n 3xn 1 3 2 6 3 9 4 12 5 15 6 18 7 21 8 24 9 27 10 30.

funcaoListaM :: Int -> String 
funcaoListaM n =  show n ++ " 3x" ++ show n ++ "\n" ++ multi
 where
  multi = funcaoConcatena [show x ++ "_" ++ (show(x*n) ++ "\n")| x <- [1..]]