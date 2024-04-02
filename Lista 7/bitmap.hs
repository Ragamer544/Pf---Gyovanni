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


--1. Defina uma função showPic que transforme uma Pic em um string que, quando
-- apresentado por putStr, apareça na tela a Pic. Assim, por exemplo, ao executar
-- putStr (showPic mulherPic)
-- deverá aparecer na tela.

showPic :: Pic -> [Pixel]
showPic pss = (concat (map barraN pss))
 where
 barraN ps = ps ++ "\n"

--2. Defina inverteBits que inverte todos pixels de uma Pic, ou seja, transforma '#' em
-- ' ' e vice-versa).
inverteBits :: Pic -> Pic
inverteBits pic = map inverteLinha pic
 where
    inverteLinha cs = map invertePixel cs
    invertePixel ' ' = '#'
    invertePixel '#' = ' ' 

-- 3. Defina flipV que transforma uma Pic espelhando na Vertical.

flipV :: Pic -> Pic
flipV pic = map reverse pic

-- 4. Defina flipH que transforma uma Pic espelhando na Horizontal.

flipH :: Pic -> Pic
flipH pic = reverse pic

--5. Defina sideBySide que junta duas Pics colocando-as lado a lado. Assuma que as duas Pics têm a mesma altura, ou seja, o mesmo número de linhas.

sideBySide :: Pic -> Pic -> Pic
sideBySide pic1 pic2 = zipWith (++) pic1 pic2


--6. Defina above que junta duas Pics colocando uma acima da outra. Assuma que as duas Pics têm a mesma largura, ou seja, o mesmo número de colunas.

above :: Pic -> Pic -> Pic
above pic1 pic2 = pic1 ++ pic2

-- 7. Defina superimpose que combina duas Pics sobrepondo-as de tal forma que os pixels
-- correspondentes são transformados assim: ‘ ‘ com ‘ ‘ se mantém ‘ ‘ enquanto qualquer
-- outra combinação resulta em ‘#’. Assuma que ambas Pics têm as mesmas dimensões.

superImpose :: Pic -> Pic -> Pic
superImpose pic1 pic2 = zipWith combinaLinhas pic1 pic2
  where
    combinaLinhas linha1 linha2 = zipWith combinaPixels linha1 linha2
    combinaPixels pixel1 pixel2
      | pixel1 == ' ' && pixel2 == ' ' = ' '  -- Espaços mantêm espaços
      | otherwise = '#'  -- Qualquer outra combinação resulta em '#'

-- 8. Defina uma função
-- scale :: Int -> Pic -> Pic
-- que escale uma Picture. Assim, por exemplo, scale 2 p resultará em uma Pic que contém
-- um quadrado de 2x2 de pixels para cada pixel em p.

scale :: Int -> Pic -> Pic
scale f pic = scaleColuna (map scaleLinha pic)
  where
    scaleColuna pic2 = concatMap (replicate f) pic2
    scaleLinha linha = concatMap (replicate f) linha

-- 9. Defina as funções
-- extendRightWith :: Char -> Int -> Pic -> Pic
-- extendLeftWith :: Char -> Int -> Pic -> Pic
-- extendAboveWith :: Char -> Int -> Pic -> Pic
-- extendBelowWith :: Char -> Int -> Pic -> Pic
-- extendHorizontallyWith :: Char -> Int -> Pic -> Pic
-- extendVerticallyWith :: Char -> Int -> Pic -> Pic
-- Que permitam estender as figuras com pixels brancos. As duas últimas funções
-- centralizam a figura original na horizontal e na vertical, respectivamente.

extendRightWith :: Char -> Int -> Pic -> Pic
extendRightWith _ 0 pic = pic
extendRightWith char n pic = map (\x -> x ++ replicate n char) pic

extendLeftWith :: Char -> Int -> Pic -> Pic
extendLeftWith _ 0 pic = pic
extendLeftWith char n pic = map (\x -> replicate n char ++ x) pic

extendAboveWith :: Char -> Int -> Pic -> Pic
extendAboveWith _ 0 pic = pic
extendAboveWith char n pic = replicate n (replicate (length (head pic)) char) ++ pic

extendBelowWith :: Char -> Int -> Pic -> Pic
extendBelowWith _ 0 pic = pic
extendBelowWith char n pic = pic ++ replicate n (replicate (length (head pic)) char)

extendHorizontallyWith :: Char -> Int -> Pic -> Pic
extendHorizontallyWith _ 0 pic = pic
extendHorizontallyWith char n pic = map (\x -> replicate n char ++ x ++ replicate n char) pic

extendVerticallyWith :: Char -> Int -> Pic -> Pic
extendVerticallyWith _ 0 pic = pic
extendVerticallyWith char n pic = replicate n t ++ pic ++ replicate n t
  where
    t = replicate (length (head pic)) char

-- 10. Estenda as definições de sideBySide e above de tal forma as as Pics possam ter
-- quaisquer dimensões. Se as dimensões não forem compatíveis, use antes
-- extendHorizontallyWith ou extendVerticallyWith para ajustar as
-- dimensões.

sideBySide' :: Char -> Pic -> Pic -> Pic
sideBySide' char pic1 pic2 = zipWith (++) pic1' pic2'
 where
  pic1'
   | length pic1 < length pic2 = (extendVerticallyWith char (length pic2 - length pic1) pic1) 
   | otherwise = pic1
  pic2'
   | length pic1 > length pic2 =  (extendVerticallyWith char (length pic1 - length pic2) pic2)
   | otherwise = pic2

above' :: Char -> Pic -> Pic -> Pic
above' char pic1 pic2 = pic1'' ++ pic2''
 where
  pic1''
   | length (head pic1) < length (head pic2) =  (extendHorizontallyWith char (length (head pic2) - length (head pic1)) pic1) 
   | otherwise = pic1
  pic2''
   | length (head pic1) > length (head pic2) =  (extendHorizontallyWith char (length (head pic1) - length (head pic2)) pic2)
   | otherwise = pic2
