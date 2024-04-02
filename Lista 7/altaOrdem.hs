-- 2. Refaça a lista de exercícios sobre imagens bitmaps – do tipo Pic. Porém, desta vez, explore
-- funções de alta ordem: parametrização parcial, seções, composição de funções e expressões
-- lambda. Tente não usar recursão nem compreensões recorrendo a funções pré-definidas tais
-- como map, filter, foldr, zipWith, etc.

type Pixel = Char -- só dois valores válidos: ' ' e '#'
type Pic = [[Pixel]]

mulherPic = ["       ###         ",
             "       ###         ",
             "#       #       #  ",
             " #     ###    #    ",
             "      #####        ",
             "     #######       ",
             "    #########      ",
             "      #  #         ",
             "     ##  ##        "]

showPic :: Pic -> IO ()
showPic pss = putStr $ concat $ map (\x -> x ++ "\n") pss


inverteBits :: Pic -> Pic
inverteBits pic = map inverteLinha pic
 where
    inverteLinha cs = map (\x -> if x == ' ' then '#' else ' ') cs

flipV = map reverse 

flipH = reverse  

extendRightWith :: Char -> Int -> Pic -> Pic
extendRightWith _ 0 pic = pic
extendRightWith char n pic = map (\x -> x ++ replicate n char) pic

extendLeftWith :: Char -> Int -> Pic -> Pic
extendLeftWith _ 0 pic = pic
extendLeftWith char n pic = map (\x -> replicate n char ++ x) pic

extendHorizontallyWith :: Char -> Int -> Pic -> Pic
extendHorizontallyWith _ 0 pic = pic
extendHorizontallyWith char n pic = map (\x -> replicate n char ++ x ++ replicate n char) pic



-- 5. Considerando a definição de árvore binária de busca vista em aula, defina funções para:
-- a. Calcular a profundidade da árvore – isto é, o comprimento do maior caminho
-- desde a raiz até uma folha.
data Tree a = Empty | Node a (Tree a) (Tree a) 
  deriving  (Eq, Show, Read)

prof :: Tree a -> Int
prof Empty = 0
prof (Node _ left right) = 1 + max  (prof left) (prof right)
-- b. Calcular o menor elemento da árvore

menor (Node x Empty right ) = x
menor (Node x left Empty)  = min x (menor left)
menor (Node x left right ) = min x $ min (menor left) (menor right)
 
-- c. Calcular o maior elemento
maior (Node x left Empty)  = x 
maior (Node x Empty right) = max x (maior right)
maior (Node x left right)  = max x $ max (maior left) (maior right)
 
-- d. Somar todos os elementos da árvore
somaArv (Node x Empty Empty) = x
somaArv (Node x left right) = x + (somaArv left) + (somaArv right)

-- e. Dada uma lista ordenada, transformar a lista em uma árvore binária de busca
-- balanceada.
transfArv :: [a] -> Tree a
transfArv []  = Empty
transfArv [x] = Node x Empty Empty
transfArv ps  = Node y (transfArv xs) (transfArv ys)
  where
    (xs,y:ys) = splitAt (div (length xs) 2) ps

transfList :: Tree a -> [a]
transfList Empty = []
transfList (Node x Empty Empty) = [x]
transfList (Node x left Empty) = transfList left ++ [x]
transfList (Node x Empty right) = x : transfList right
transfList (Node x left right) = transfList left ++ [x] ++ transfList right


exemplo = Node 0 (Node 5 (Node 8 (Node 9 Empty Empty) 
                                 (Node 6 Empty Empty)) 
                         (Node 2 (Node 3 Empty Empty) Empty)) 
                 (Node (-6) (Node (-3) (Node (-2) (Node (-1) Empty Empty) Empty) 
                                       (Node (-5) (Node (-4) Empty Empty) Empty)) 
                            (Node (-9) (Node (-8) (Node (-7) Empty Empty) Empty) 
                                       (Node (-10) Empty Empty)))



-- f. Desafio opcional: Eliminar um dado elemento da árvore. Se o elemento não
-- está na árvore, a árvore é inalterada.

eliminaOcorrencia :: (Ord a) => a -> Tree a -> Tree a
eliminaOcorrencia _ Empty = Empty
eliminaOcorrencia x (Node y left right)
  | x < y = Node y (eliminaOcorrencia x left) right
  | x > y = Node y left (eliminaOcorrencia x right)
  | otherwise = removeNode (Node y left right)
  where
    removeNode :: Tree a -> Tree a
    removeNode Empty = Empty
    removeNode (Node _ Empty right) = right
    removeNode (Node _ left Empty) = left
    removeNode (Node _ left right) =
      let (successor, newRight) = deleteMin right
      in Node successor left newRight

    deleteMin :: Tree a -> (a, Tree a)
    deleteMin (Node x Empty right) = (x, right)
    deleteMin (Node x left right) =
      let (minVal, newLeft) = deleteMin left
      in (minVal, Node x newLeft right)


data Shape = Circle Float | Rectangle Float Float


area :: Shape -> Float
area (Circle r) = 3.14 * r^2
area (Rectangle b h) = b * h