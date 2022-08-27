--ALUNO: RAFAEL BAUER SAMPAIO

--1. Escreva uma função chamada soma1 que recebe um inteiro como argumento e retorna um inteiro uma unidade maior que a entrada.
soma1 :: Int -> Int --assinatura
soma1 x = x+1 --declaração

--2. Escreva uma função chamada sempre que, não importando o valor de entrada, devolva sempre zero. Observe que neste caso a entrada pode ser de qualquer tipo.
sempre :: a -> Int
sempre x = 0

--3. Escreva uma função chamada treco que receba três valores em ponto flutuantes com precisão dupla e retorne o resultado da soma dos dois primeiros multiplicado pelo terceiro.
treco :: Float -> Float -> Float -> Float
treco x y z = (x+y)*z

--4. Escreva uma função chamada resto que devolva o resto de uma divisão entre dois números inteiros.
resto :: Int -> Int -> Int
resto x y = rem x y

--5. Escreva uma função chamada precoMaior que devolva o maior valor entre quatro valores monetários.
precoMaior :: Float -> Float -> Float -> Float -> Float
precoMaior x y z j = maximum[x,y,z,j]

--6. Escreva uma função chamada impar que devolva True, sempre que o resultado do produto de dois números inteiros for ímpar.
impar :: Int -> Int -> Bool
impar x y = not (even(x*y))

--7. Em Haskell existe o tipo par cuja assinatura tem a seguinte forma: 𝑝𝑎𝑟∷(𝐼𝑛𝑡,𝐼𝑛𝑡). Escrevauma função em Haskell que devolva a soma dos componentes de um par de inteiros.
parInteiros :: (Int,Int) -> Int
parInteiros (x,y) = x+y

--8. Escreva uma função em Haskell que receba números reais (double) e devolva o resultadoda equação 𝑥2 +𝑦2 +𝑧.
equacao :: Double -> Double -> Double -> Double
equacao x y z = (x^2)+y/2+z

--9. Escreva uma função em Haskell chamada diagnostico que receba o peso e a altura do aluno e imprima um diagnóstico de obesidade.
diagnostico :: Double -> Double -> String
diagnostico x y
  |x/y**2<17 = "Muito abaixo do peso"
  |x/y**2>=17 && x/y**2<18.49 = "Abaixo do peso"
  |x/y**2>=18.50 && x/y**2<24.99 = "Peso normal"
  |x/y**2>=25 && x/y**2<29.99 = "Sobrepeso"
  |x/y**2>=30 && x/y**2<34.99 = "Obesidade leve"
  |x/y**2>=35 && x/y**2<39.99 = "Obesidade severa"
  |otherwise = "Obesidade morbida"

--10. Escreva uma função em Haskell chamada bissexto que receba um ano e devolva True se o ano for bisexto.
bissexto :: Int -> Bool
bissexto x
  |mod x 400 == 0 = True
  |mod x 100 == 0 = False
  |mod x 4 == 0 = True
  |otherwise = False

main = do
  print("1. Soma")
  print(soma1 5)
  print("2. Sempre 0")
  print(sempre 85)
  print("3. Treco")
  print(treco 2.24 4.65 6.95)
  print("4. Resto")
  print(resto 15 2)
  print("5. Preco maior")
  print(precoMaior 2.50 3.27 56.12 35.25)
  print("6. Impar")
  print(impar 9 3)
  print("7. Par de inteiros")
  print(parInteiros (2,3))
  print("8. Equacao")
  print(equacao 4 3 2)
  print("9. Diagnostico")
  print(diagnostico 71 1.73)
  print("10. Bissexto")
  print(bissexto 2022)
