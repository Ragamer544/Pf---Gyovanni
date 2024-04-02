--                                    Atividade guardas PF                                    --

-- Defina uma função que receba três números e determine se todos são diferentes.

dif3 :: Int -> Int -> Int -> Bool
dif3 x y z
  | x /= y && y /= z && x /= z = True
  | otherwise = False

-- Defina uma função que receba três números e determine o menor deles.

menor3 :: Int -> Int -> Int -> Int
menor3 a b c
  | a <= b && a <= c = a
  | b <= c = b
  | otherwise = c

-- Defina uma função que receba três números e determine quantos estão acima da média dos três.

maiormedia3 :: Float -> Float -> Float -> Int
maiormedia3 a b c
  | a > m3 && b > m3 || a > m3 && c > m3 || b > m3 && c > m3 = 2
  | a > m3 || b > m3 || c > m3 = 1
  | a == b && b == c = 0
  where
    m3 = (a + b + c) / 3

--  Defina uma função que calcule a mediana de três números. A mediana é o número que fica no meio quando os números são ordenados.

mediana3 :: Int -> Int -> Int -> Int
mediana3 a b c
  | a <= b && a >= c || a >= b && a <= c = a
  | b <= a && b >= c || b >= a && b <= c = b
  | otherwise = c

-- Defina uma função que receba os coeficientes 𝑎, 𝑏 e 𝑐 de uma equação de segundo grau 𝑎𝑥2 + 𝑏𝑥 + 𝑐 = 0 e que retorne o número de raízes da equação. Assuma que 𝑎 é diferente de zero. de zero.

numeroDeRaizes :: Float -> Float -> Float -> Int
numeroDeRaizes a b c
  | delta > 0 = 2 -- Duas raízes reais distintas
  | delta == 0 = 1 -- Uma raiz real (raíz dupla)
  | otherwise = 0 -- Nenhuma raiz real
  where
    delta = b ^ 2 - 4 * a * cornar 3.

-- Defina uma função que receba os coeficientes 𝑎, 𝑏 e 𝑐 de uma equação de segundo grau 𝑎𝑥 2 + 𝑏𝑥 + 𝑐 = 0 e que retorne o número de raízes da equação. Qualquer coeficiente pode ser zero. Se a  equação tiver infinitas soluções, a função deve retornar 3.

tipoRaizes :: Float -> Float -> Float -> Int
tipoRaizes a b c
  | a == 0 && b == 0 && c == 0 = 3 -- Equação com coeficientes nulos (infinitas soluções)
  | a == 0 && b == 0 = 0 -- Equação impossível (sem soluções)
  | a == 0 && b /= 0 = 1 -- Equação linear (uma solução)
  | delta > 0 = 2 -- Duas raízes reais distintas
  | delta == 0 = 1 -- Raiz real dupla
  | otherwise = 0 -- Nenhuma raiz real (raízes complexas)
  where
    delta = b ^ 2 - 4 * a * c

-- A Locadora de Veículos Eudora lançou uma grande promoção esse mês: pagando apenas R$ 90 por diária, o cliente pode alugar um carro de passeio. Para cada diária, o cliente recebe uma cota de quilometragem de 100 Km. Cada quilômetro a mais custará uma taxa extra de R$ 12. Escreva uma função que receba como entrada a quantidade de dias e a quilometragem total rodada por um cliente dessa locadora e retorne o valor total  a ser pago.

calcularvalor :: Int -> Int -> Float
calcularvalor d km = fromIntegral ((90 * d) + tx d km)
  where
    tx d km =
      if km <= (100 * d)
        then 0
        else (12 * (km - (100 * d)))

-- Chama-se ano bissexto o ano ao qual é acrescentado um dia extra, ficando ele com 366 dias, um dia a mais do que os anos normais de 365 dias, ocorrendo a cada quatro anos,exceto anos múltiplos de 100 que não são múltiplos de 400. Defina uma função que, dado um ano, indique se este é bissexto.

anobissexto :: Int -> Bool
anobissexto a
  | (mod a 400 == 0) = True
  | (mod a 100 == 0) = False
  | (mod a 4 == 0) = True
  | otherwise = False

-- Uma corretora de seguros cobra mais barato se o principal condutor do veículo é mulher e se tem mais de 40 anos. Caso contrário, o valor do seguro fica caro. Defina uma função que receba um valor booleano que indica se o condutor é homem ou mulher (True ou False, respectivamente), outro valor inteiro com a idade em anos do condutor e que retorna um booleano para indicar se o seguro vai ficar barato ou caro (True ou False),respectivamente.

valorseguro :: Char -> Int -> Bool
valorseguro a b
  | a == 'm' && b >= 40 = True
  | otherwise = False