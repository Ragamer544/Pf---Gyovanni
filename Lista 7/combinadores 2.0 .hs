import Data.Char

-- 1. Defina funções para:
-- ● Eliminar espaços no início do texto. Consideramos que um caractere é um espaço
-- quando satisfaz a propriedade isSpace do módulo Data.Char.
-- pulaDemarcadores :: -> String -> String

pulaDemarcadores ::  String -> String
pulaDemarcadores xs = dropWhile isSpace xs

-- ● Coletar a primeira palavra do texto. Você deve supor que a palavra é composta
-- somente por caracteres que não são espaços.
-- primPalavra :: -> String -> String

primPalavra :: String -> String
primPalavra xs = takeWhile (/= ' ') xs

-- ● Descartar a primeira palavra do texto.
-- descPrimPal :: -> String -> String

descPrimPal :: String -> String
descPrimPal xs = dropWhile (== ' ') xs

-- ● Coletar todas as palavras do texto, gerando uma lista de palavras. Use recursão na
-- definição desta definição.
-- listaPalavras :: -> String -> [String]

listaPalavras :: String -> [String]
listaPalavras texto = reverse (go [] [] texto)
  where
    go :: [String] -> String -> String -> [String]
    go palavras palavraAtual [] = palavraAtual : palavras
    go palavras palavraAtual (c:cs)
      | isAlphaNum c = go palavras (palavraAtual ++ [c]) cs
      | otherwise =
        if null palavraAtual
          then go palavras palavraAtual cs
          else go (palavraAtual : palavras) [] cs

-- 2. Usando a função foldr ou foldr1, elabore:

-- ● Função para efetuar o or de uma lista de Booleanos.

or' xs = foldr (||) False xs

-- ● Função para realizar o produto de uma lista de reais.

prod xs = foldr (*) 1 xs 

-- ● Função para calcular o fatorial de um número inteiro.

fat x = foldr (*) 1 [1..x]

-- ● Função para calcular o maior elemento de uma lista.

maiorElem xs = foldr1 max xs

-- 3. Usando somente foldr, defina funções para:

-- ● Calcular o produto dos quadrados de uma lista.
quad xs = foldr prodquad 1 xs
 where
    prodquad x y = x^2 * y^2 

-- ● Calcular a soma dos ímpares de uma lista.
somaIm xs = foldr impar 0 xs
 where
    impar x y = if odd x then x + y else y

-- ● Calcular o produto dos quadrados dos números maiores que 3 de uma lista de inteiros.

prod' xs = foldr soma 1 xs
 where
  soma x y = if x > 3 then x^2 * y else y

--4. Defina as funções map e filter usando foldr.

--usando lambda :
map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x acc -> f x : acc) []

--normal :
map'' f xs = foldr r [] xs
 where
  r x y = (f x ): y

--usando lambda :
filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\x acc -> if p x then x : acc else acc) []

--normal :

filter'' p xs = foldr r [] xs
 where
  r x y = if p x then x : y else y

--5. Usando foldr, defina as funções do Prelude unzip, last e init.

unzip' :: [(a, b)] -> ([a], [b])
unzip' = foldr (\(x, y) (xs, ys) -> (x:xs, y:ys)) ([], [])

last' :: [a] -> a
last' = foldr (\x _ -> x) undefined

init' :: [a] -> [a]
init' = foldr (\x xs -> if null xs then [] else x : init' xs) []