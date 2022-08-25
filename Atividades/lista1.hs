import Data.Char
{- 1. Declare, em Haskell, as funções abaixo, contemplando, também, os protótipos (cabeçalhos): -}
--(a)
f1::Int->Int
f1   x
 | x >= 0 = div (x+4) (x+2)
 | otherwise = div 2 x
--(b)
f2::(Int,Int)->Int 
f2  (x,y)
 | x >= y = x+y
 | otherwise = x-y
--(c)
f3::(Int,Int,Int)->Int
f3  (x,y,z)
 | (x+y) > z = x+y+z
 | (x+y) < z = x-y-z
 | otherwise = 0

{- 2. Localize, explique e corrija o erro na função que deve calcular o fatorial de um número, como se segue:
      fat::Int->Int
      fat x = x * fat(x-1) -}
{- R.: Toda função recursiva deve apresentar uma base que servirá de critério de parada para a recursividade. Ademais, "fat" 
deve limitar a entrada, uma vez que não existe fatorial de um inteiro negativo. Portanto, a correção se daria por: -}

fat::Int->Int
fat   0 = 1 -- critério de parada
fat   x = x * fat (x-1)

{- 3. Considere a função em Haskell soma::Int->Int->Int que retorna a soma entre os dois parâmetros. Assim, faça uma função em 
Haskell que resulte a multiplicação de dois parâmetros fazendo uso da função soma. -}
soma::Int->Int->Int
soma   a    b = a+b

multiplica::Int->Int->Int
multiplica   a    1 = a
multiplica   a    b = soma a (multiplica a (b-1))

{- 4. Escreva, em Haskell, a função invertInt::Int->Int que inverta os dígitos de um número inteiro. -}
listSize::[dataType]->Int
listSize   [] = 0 
listSize  (h:t) = 1 + listSize t

listToInt::[Int]->Int
listToInt  [] = 0
listToInt  (h:t) = (h * 10^(listSize t)) + listToInt t 

reverseList::[Int]->[Int]
reverseList   [] = []
reverseList   (h:t) = (reverseList t) ++ h:[]

createList::Int->[Int]
createList   0 = []
createList   x = createList (div x 10) ++ (mod x 10):[]

invertInt::Int->Int
invertInt   x 
        | x >=0 = listToInt (reverseList (createList x))
        | otherwise = (-1) * listToInt (reverseList (createList (abs x)))

{- 5. Escreva, em Haskell, a definição de uma função fourPower que retorne o seu argumento elevadoà quarta potência.  
Use a função square dada em sala de aula na definição de fourPower. -}
square::Int->Int
square   x = x*x

fourPower::Int->Int
fourPower   x = square (square x)

{- 6. Considere a sequência: √6;√(6 +√6);√[6 +√(6 +√6)]; ...; com tendência ao +∞. Faça, em Haskell,uma função para calcular o  
i−ésimo termo desta sequência, considerando i0 =√6.-}
ithTerm::Int->Double
ithTerm   0 = sqrt 6
ithTerm   i = sqrt (6 + ithTerm (i-1))

907230297

--O resultado consiste em tirar a raiz de xi até x0 e somar os termos/ OBS.: encontra o i-esimo termo de uma Progressão Geométrica 
-- ithTerm::Int->[Int]
-- ithTerm   0 = 6:[]
-- ithTerm   i = 6:[] ++ ithTerm (i-1)

{- 7. Escreva, em Haskell, uma função que informa de quantas maneiras é possível escolher n objetos em uma coleção original de m  
objetos, para m ≥ n. -}
combinacaoNaN::Int->Int->Int
combinacaoNaN   n    m
            | m >= n = div (fat m) ((fat n)*(fat (m-n)))
            | otherwise = 0

{- 8. Considere a função escrita na linguagem C que calcula o máximo denominador comum entre dois números:
      int mdc(int m, int n) {
          while ((m \% n) != 0) {
              int aux = m;
              m = n;
              n = aux \% n ;
          }
          return n;
      }
Escreva uma função, em Haskell, que calcule o MDC de maneira recursiva. -}
mdc::Int->Int->Int
mdc   m    n
  | r /= 0 = mdc n r
  | otherwise = n
    where r = mod m n

{- 9. Escreva, em Haskell, uma função que retorna quantos múltiplos de um determinado inteiro tem em um intervalo fornecido. 
Por exemplo, o número 4 tem 2 múltiplos no intervalo de 1 a 10. -}
howManyMultiples::Int->Int->Int->Int
howManyMultiples  num lim1 lim2 
               | lim2 < num = 0
               | mod lim2 num == 0 = 1 + howManyMultiples num lim1 (lim2-1)
               | otherwise = howManyMultiples num lim1 (lim2-1)

{- 10. Escreva, em Haskell, uma função que retorna o último dígito de um número inteiro. -}
lastDigit::Int->Int
lastDigit   x = mod (abs x) 10

{- 11. Escreva, em Haskell, uma função que retorna o dígito de um número inteiro de acordo com a posição informada. -}
findElement::Int->[Int]->Int
findElement   _    [] = (-1) 
findElement   0   (h:t) = h
findElement index (h:t) = findElement (index-1) t
          
anyDigit::Int->Int->Int
anyDigit index  n = findElement index (createList n) 

{- 12. Um programador especificou a função allDifferent para identificar se três números inteiros são todos diferentes entre si, 
da seguinte forma: 
      allDifferent::Int->Int->Int->Bool
      allDifferent m n p = (m/=n) && (n/=p) -}
--(a) O que está errado nessa definição?
-- R.: A equação lógica não cobre o cenário em que m é igual. Portanto, (m,n,p) = (2,3,2) retornará true, fato que é falso.

--(b) Especifique corretamente uma função allDifferent para o propósito necessário
allDifferent::Int->Int->Int->Bool
allDifferent   m    n    p = not ((m == n) || (m == p) || (n == p))

{- 13. Escreva uma função howManyEqual que retorne quantos dos três números inteiros fornecidos
como argumentos são iguais. A resposta poderá ser 3 (todos iguais), 2 (dois iguais e o terceiro
diferente) ou 0 (todos diferentes). -}

howManyEqual::Int->Int->Int->Int
howManyEqual   a    b    c
           | (a == b) && (b == c) = 3
           | (a == b) || (a == c) || (b == c) = 2
           | otherwise = 0
      
{- 14. Para o exemplo da função sales::Int->Int dada em sala de aula faça o que se pede: -}
{- (a) Implemente a função howManyLess que calcule quantos dias as vendas foram inferiores a um dado valor, dentro de um intervalo 
de dias dentro do período total. O primeiro parâmetro de howManyLess indica o valor mínimo de vendas, o segundo parâmetro indica
o dia do início do intervalo e o terceiro parâmetro é o dia do fim do intervalo desejado dentro do período total de dias da função;
-}
sales :: Int -> Int
sales 1 = 41
sales 2 = 72
sales 3 = 41
sales 4 = 2
sales 5 = 91
sales 6 = 55
sales 7 = 41
sales _ = 0

howManyLess::Int->Int->Int->Int
howManyLess value beg  end
          | end == (beg-1) = 0
          | value == (sales beg) = 1 + howManyLess value (beg+1) end
          | otherwise = howManyLess value (beg+1) end

{- (b) Implemente a função noZeroInPeriod::Int->Bool que retorna True somente se não há nenhum dia no período em que o número de 
vendas da função sales foi zero. -}
noZeroInPeriod::Int->Bool
noZeroInPeriod   0 = True 
noZeroInPeriod   n = ((sales n /= 0) && noZeroInPeriod (n-1))

{- (c) Implemente a função zerosInPeriod::[Int] que retorne a lista de todos os dias em que as vendas foram de zero unidades; -}
equalsTo::Int->Int->Bool
equalsTo   a    b = a == b

matchSalesToValue::(Int->Int->Bool)->Int->Int->[Int]
matchSalesToValue         _           _    0 = []
matchSalesToValue         f         value day
                | f (sales day) value = matchSalesToValue f value (day-1) ++ [day]
                | otherwise = matchSalesToValue f value (day-1)

zerosInPeriod::[Int]
zerosInPeriod = matchSalesToValue equalsTo 0 7

{- (d) Utilizando listas de inteiros, retorne os dias em que as vendas foram abaixo de um determinado valor passado como parâmetro;
-}
lessThan::Int->Int->Bool
lessThan   a    b = a < b

lessThanValueInPeriod::Int->[Int]
lessThanValueInPeriod value = matchSalesToValue lessThan value 7

{- 15. A sequencia de Fibonacci é definida e conhecida na literatura. Os dois primeiros números são 0 e 1, e os seguintes são 
calculados como a soma dos dois anteriores na sequência. Defina a função antFib que, dado um valor x, calcule a posição de x na 
sequencia de Fibonacci. Caso x não esteja na sequência, retorne (-1). -}
fibonacci::Int->Int
fibonacci   0 = 0
fibonacci   1 = 1
fibonacci   x = fibonacci (x-1) + fibonacci (x-2)

findFibonacci::Int->Int->Int
findFibonacci value  n
            | fibonacci n < value = findFibonacci value (n+1)
            | fibonacci n == value = n
            | otherwise = -1

antFib::Int->Int
antFib value = findFibonacci value 0

{- 16. Escreva uma definição equivalente à exibida abaixo, mas usando apenas uma única cláusula
em casamento de padrão: 
      funny x y z
          | x > z = True
          | y >= x = False
          | otherwise = True 
-}
funny x y z = (x > z) || (y < x)   

{- 17. Implemente uma função que converte uma letra minúsculas como entrada para seu equivalente em maiúsculo. Caso a entrada não 
seja uma letra minúscula, retorne o próprio caractere de entrada. Como dica, veja a função predefinida isLower::Char->Bool. Para 
verificar outras funções pré-definidas para o tipo Char, consulte a biblioteca padrão no endereço 
http://zvon.org/other/haskell/Outputglobal/index.html. -}
upperToLower::Char->Char
upperToLower   ch
           | not (isLower ch) = chr (ord ch + (97-65))
           | otherwise = ch

{- 18. Defina uma função charToNum::Char->Int que converte um dígito numérico do tipo Char (como '3') para o valor que ele 
representa em Int, (3). Se o caractere de entrada não representa um dígito numérico, a função deve retornar -1. Como dica, veja as 
funções isDigit, chr e ord do módulo Data.Char. -}
charToNum::Char->Int
charToNum   ch
        | isDigit ch = ord ch - 48
        | otherwise = -1

{- 19. Implemente a função duplicate::String->Int->String que recebe uma string s e um número inteiro n. A função deve retornar a 
concatenação de n cópias de s. Se n for zero, retorna "". Como dica, usar o operador de concatenação pré-definido 
(++)::String->String->String. -}
duplicate::String->Int->String
duplicate    _      0 = ""
duplicate   str     x = str ++ duplicate str (x-1)

{- 20. Implemente a função pushRight::String->Int->String que recebe uma string s e um número inteiro n e retorna uma nova string 
t com k caracteres '>' inseridos no início de s. O valor de k deve ser tal que o comprimento de t seja igual a n. Obs: se n é menor 
que o comprimento de s, a função retorna a própria string s. -}
pushRight::String->Int->String
pushRight   str     n
        | listSize str < n = '>': pushRight str (n-1)
        | otherwise = str

{- 21. Defina um operador binário de nome &-, com a semântica: x &- y = x - 2*y. -}
-- APRENDEREMOS DEPOIS DA PROVA

{- 22. Faça em Haskell uma solução para inverter os elementos de uma lista de Inteiros. -}
inverte::[Int]->[Int]
inverte   list = reverseList list

{- 23. Faça em Haskell uma solução para, dada uma lista de inteiros, retornar uma dupla de listas de inteiros onde a primeira 
conterá os elementos ímpares e a segunda os elementos pares passados como parâmetro. -}
splitOddEven::[Int]->([Int],[Int])->([Int],[Int])
splitOddEven    []     (odd,even)      = (odd,even) 
splitOddEven  (h:t)    (odd,even)
           | mod h 2 == 0 = splitOddEven t (odd++[h],even)
           | otherwise = splitOddEven t (odd,even++[h])

splitOddEvenInterface::[Int]->([Int],[Int])
splitOddEvenInterface   list = splitOddEven list ([],[])

{- 24. Faça em Haskell uma solução para, dada uma lista de inteiros, retornar a string contendo as letras do alfabeto cuja posição 
é dada pelos elementos da lista. -}
convertPositionToAlphabet::[Int]->String
convertPositionToAlphabet    [] = ""
convertPositionToAlphabet  (h:t) = (chr (h + 64)): convertPositionToAlphabet t

{- 25. Sabendo que [1..7] é equivalente à lista [1,2,3,4,5,6,7], complete as correspondências abaixo: 
   (a) ['a'..'g'] = "abcdefg"
   (b) [0.1 ..0.9] = [0.1,1.1]
   (c) [0.1,0.3 .. 0.9] = [0.1,0.3,0.5,0.7,0.9]
   (d) [0.1,0.3 ..1.8] = [0.1,0.3,0.5,0.7,0.9,1.1,1.3,1.5,1.7,1.9]
   (e) [0.4,0.2 ..0.8] = []
   (f) [1,4..15] = [1,4,7,10,13]
-}

{- 26. Faça em Haskell uma solução para o seguinte problema: Dada uma lista de caracteres [Char],
e um caractere a, retornar quantos caracteres da lista são iguais a a. -}
scanSimilar::[Char]->Char->Int
scanSimilar    []     _  = 0
scanSimilar  (h:t)    ch
          | h == ch = 1 + scanSimilar t ch
          | otherwise = scanSimilar t ch

{- 27. Para uma lista de elementos inteiros ordenada qualquer, faça uma função que retorne uma lista
de inteiros ordenada sem elementos repetidos. -}
teste = removeRepeatedElements [1,2,3,4]

removeRepeatedElements::[Int]->[Int]
removeRepeatedElements  (h:[]) = h:[]
removeRepeatedElements  (a:b:t)
                     | b:t == [] = a:[]
                     | a == b = removeRepeatedElements (a:t)
                     | otherwise = a:removeRepeatedElements (b:t)

{- 28. Faça uma solução em Haskell que, dada uma lista de inteiros, ela retorne uma lista com uma
repetição de cada elemento de acordo com seu valor. -}
adicionaNVezes::dataType->Int->[dataType]
adicionaNVezes     _       0 = []
adicionaNVezes     x count = x:adicionaNVezes x (count-1)

adicionaIntNVezesInterface::Int->[Int]
adicionaIntNVezesInterface  num = adicionaNVezes num num

proliferaInt::[Int]->[Int]
proliferaInt   []  = []
proliferaInt  (h:t) = adicionaIntNVezesInterface h ++ proliferaInt t 

{- 29. Faça uma solução em Haskell que, dada uma lista de caracteres maiúsculos, ela retorne uma
lista com uma repetição de cada elemento de acordo com o valor de sua ordem no alfabeto. -}
proliferaChar::[Char]->[Char]
proliferaChar    []  = []
proliferaChar  (h:t) = adicionaNVezes h ((ord h)-64) ++ proliferaChar t
---------------------------------------------------------------------------------------------------------------------------------
