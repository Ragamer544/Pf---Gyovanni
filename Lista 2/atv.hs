-- Lista de exercicios aula --

-- escreva uma função, que determina se um numero é impar, par , positivo, negativo ou nulo. saida deve ser string. Um exemplo é ''impar positivo''

tiponum :: Int -> String
tiponum a
  | a == 0 = "o numero é nulo"
  | a /= 0 = "o número é " ++ sinal a ++ paridade a
  | otherwise = "não é um número"
  where
    sinal a
      | a > 0     = "positivo e "
      | otherwise = "negativo e "
    paridade a
      | even a     = "par"
      | otherwise   = "ímpar"

--Anualmente, o MEC avalia os cursos universitários de todo o país e atribui conceitos com base em diversos critérios, sendo um deles a quantidade de livros disponíveis. Considerando as regras definidas abaixo, escreva uma função que receba como entrada a quantidade de livros e a quantidade de alunos de um curso, ambos inteiros, e retorne a letra maiúscula correspondente ao conceito concedido pelo MEC. 1 livro para até 8 alunos --> Conceito A ; 1 livro para entre 9 e 12 alunos --> Conceito B ; 1 livro para entre 13 e 18 alunos --> Conceito C ; 1 livro para mais de 18 alunos --> Conceito D

conceitomec :: Int -> Int -> Char
conceitomec livro aluno
  | alunoporlivro <= 8  ='A'
  | alunoporlivro <= 12 ='B'
  | alunoporlivro <= 18 ='C'
  | alunoporlivro > 18  ='D'
  where
    alunoporlivro = div aluno livro

--Escreva uma função para classificar um triângulo em: escaleno (os três lados de comprimentos diferentes), isósceles (dois lados de comprimentos iguais) ou equilátero (os três lados de comprimentos iguais). A função receberá como dados os tamanhos dos três lados. A função também deverá estabelecer se os três lados efetivamente formam um triângulo.

classificartriangulo :: Int -> Int -> Int -> String
classificartriangulo a b c
  | a >= b+c || b >= a+c || c >= a+b = "Nao formam um triangulo"
  | a == b && b == c = "Triangulo equilatero"
  | a == b && b /= c = "Triangulo isosceles"
  | otherwise = "Triangulo escaleno"

--Defina uma função que calcule quantos dias faltam para o próximo São João. A função receberá como entradas três inteiros correspondentes ao dia, mês e ano atuais. A solução deve considerar que um ano pode ser bissexto.
bissexto:: Int -> Bool
bissexto b
 | mod b 4   /=0                  = False
 | mod b 100 ==0 && mod b 400 /=0 = False
 | otherwise                      = True

dataatual :: Int -> Int -> Int -> Int
dataatual dia mes ano
  |mes==1   =dia
  |mes==2   =dia+31
  |mes==3   =dia+31+fev
  |mes==4   =dia+62+fev
  |mes==5   =dia+92+fev
  |mes==6   =dia+123+fev
  |mes==7   =dia+153+fev
  |mes==8   =dia+184+fev
  |mes==9   =dia+215+fev
  |mes==10  =dia+245+fev
  |mes==11  =dia+276+fev
  |mes==12  =dia+306+fev
    where
        fev= if   bissexto ano
             then 29
             else 28

proxsaojoao:: Int -> Int -> Int -> Int
proxsaojoao d m a
  |dataatual 24 6 a >= dataatual d m a = dataatual 24 6 a - dataatual d m a
  | otherwise                          = dataatual 31 12 a - dataatual d m a +  proxsaojoao 24 6 (a+1)

  --Rafinha sabe que em sua cidade o valor da KWh de energia varia da forma mostrada abaixo. Até 99 KWh: R$1.35 100 até 299 KWh: R$1.55 300 até 574 KWh: R$1.75 Maior ou igual a 575 KWh: R$2.15 Ele também sabe que quando o consumo é maior que 300KWh é cobrado uma taxa de 10% no valor da conta e o preço mínimo de qualquer conta é R$35. Escreva uma função para auxiliar Rafinha a calcular o valor de sua conta elétrica.

valorconta :: Float -> Float
valorconta consumo

  | consumo <= 99  || consumo <= 299 || consumo > 300 = valorKWh consumo + taxaExtra (valorKWh consumo)
  | otherwise = if valorKWh consumo > 35 then valorKWh consumo else 35
  where
    valorKWh consumo
      | consumo < 100 = 1.35 * consumo
      | consumo < 300 = 1.55 * consumo
      | consumo < 575 = 1.75 * consumo
      |otherwise      = 2.15 * consumo
    taxaExtra valor
      | consumo * 0.1 > 35 =  consumo * 0.1
      | otherwise = 35

--Uma estratégia para resolver problemas é primeiro lidar com simplificações do problema original. Neste exercício você deve definir uma função para calcular idade em anos, meses e dias. Antes de definir a solução completa, defina: a. Uma função que calcula a idade somente em anos completos. b. Uma função que calcula a idade em anos e meses completos. c. A função requerida. A solução deve considerar anos bissextos.

calcularIdadeano :: Int -> Int -> Int -> Int -> String
calcularIdadeano mesatual anoatual mesnascimento anonascimento
 |mesatual > mesnascimento = "tem " ++ show (anoatual-anonascimento) ++ " anos"
 | otherwise               = "tem " ++ show (anoatual-anonascimento-1) ++ " anos"