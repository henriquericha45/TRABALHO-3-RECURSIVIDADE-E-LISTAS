-- Henrique Levandoski Richa

{- 1. Escreva uma função para o cálculo dos números da sequência de Fibonacci, utilizando
Haskell. -}
fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci x = fibonacci (x -1) + fibonacci (x -2)

{- 2. Um dos primeiros algoritmos documentados é o algoritmo para o cálculo do Maior Divisor
Comum (MDC) de Euclides publicado por volta do ano 300 AC. Podemos simplificar este
algoritmo dizendo que dados dois inteiros A e B, o MDC entre eles será dado pelo valor
absoluto de A se B=0 e pelo MDC entre B e o resto da divisão de A por B se B>0. Escreva
uma função para o cálculo do MDC entre dois números inteiros positivos, usando o
algoritmo de Euclides conforme apresentado aqui, utilizando Haskell. -}
mdc :: Int -> Int -> Int
mdc x 0 = x
mdc x y =
  if (x >= y)
    then mdc y (mod x y)
  else mdc x (mod y x)

{- 3. Escreva uma função recursiva que dado um número inteiro n, devolva a soma dos dígitos
deste número. Exemplo: dado 1234 a função deverá devolver 10. Utilizando Haskell e
recursividade. -}
somaDigitos :: Int -> Int
somaDigitos 0 = 0
somaDigitos x = (mod x 10) + somaDigitos (div x 10)

{- 4. Escreva uma função que devolva a soma de todos os números menores que 10000 que
sejam múltiplos de 3 ou 5. -}
somaMenoresEMultiplos :: Int -> Int
somaMenoresEMultiplos x =
  if (x == 0)
    then x
    else if ((mod x 3) == 0) || ((mod x 5) == 0)
      then x + somaMenoresEMultiplos (x -1)
    else somaMenoresEMultiplos (x -1)

{- 5. Escreva uma função que, recebendo uma lista de inteiros, apresente a diferença entre a
soma dos quadrados e o quadrado da soma destes inteiros, usando recursividade. -}
quadrado :: [Int] -> Int
quadrado [] = 0
quadrado (head:tail) = head^2 + quadrado tail

soma :: [Int] -> Int
soma [] = 0
soma (head:tail) = head + soma tail

diferencaSomaQuadrado :: [Int] -> Int
diferencaSomaQuadrado list = quadrado list - (soma list)^2

{- 6. O Crivo de Eratóstenes não é o melhor algoritmo para encontrar números primos. Crie uma
função que implemente o Crivo de Euler (Euler’s Sieve) para encontrar todos os números
primos menores que um determinado inteiro dado. -}
factors :: Int -> [Int]
factors n = [x | x <- [1..n], mod n x == 0]

prime :: Int -> Bool
prime n = factors n == [1,n]
--                                 condicao
todosOsPrimos :: Int -> [Int]
todosOsPrimos y= [x | x <- [1..y], prime x]

{- 7. Nem só de Fibonacci vivem os exemplos de recursão. Escreva uma função que devolva
todos os números de uma sequência de Lucas (2, 1, 3, 4, 7, 11, 18, 29, 47, 76, 123) menores
que um inteiro dado. -}
lucas :: Int -> Int
lucas 0 = 2
lucas 1 = 1
lucas n =
  lucas (n-1) + lucas (n-2)

sequenciaLucas :: Int -> [Int]
sequenciaLucas 0 = [0]
sequenciaLucas limit = [(lucas i) | i <- [0..limit], lucas i<limit]

{- 8. Escreva uma função, chamada aoContrario em Haskel para reverter uma lista. Dado [1,2,3]
devolva [3,2,1]. -}
aoContrario :: [Int] -> [Int]
aoContrario x = reverse x

{- 9. Escreva uma função chamada somaRecursiva que recebe dois valores inteiros e devolve o
produto destes valores sem usar o operador de multiplicação. -}
somaRecursiva :: Int -> Int -> Int
somaRecursiva x y =
  if (y == 0)
    then 0
  else x + somaRecursiva x (y -1)

{- 10. Escreva uma função chamada comprimento que receba uma lista de inteiros e devolva o
comprimento desta lista. Observe que você não pode usar nenhuma função que já calcule
o comprimento de uma lista.
 -}
comprimento :: [Int] -> Int
comprimento [] = 0
comprimento (head : tail) = 1 + comprimento tail


main = do

  putStr "Func. 1: entrada: 2; resultado: "
  print(fibonacci 5)

  putStr "Func. 2: entrada: 15 10; resultado: "
  print(mdc 15 10)

  putStr "Func. 3: entrada 1234; resultado: "
  print(somaDigitos 1234)

  putStr "Func. 4: entrada: 10000; resultado: "
  print(somaMenoresEMultiplos 10000)

  putStr "Func. 5: entrada: [3, 5]; resultado: "
  print(diferencaSomaQuadrado [3, 5])

  putStr "Func. 6: entrada: 150; resultado: "
  print(todosOsPrimos 150)

  putStr "Func. 7: entrada: 15; resultado: "
  print(sequenciaLucas 15)

  putStr "Func. 8: entrada: [1,2,3]; resultado: "
  print(aoContrario [1,2,3])

  putStr "Func. 9: entrada: 5 4; resultado: "
  print(somaRecursiva 5 4)

  putStr "Func. 10: entrada: [1,2,3,4,5,6]; resultado: "
  print(comprimento [1,2,3,4,5,6])