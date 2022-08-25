import Data.Char

{- 1. Defina uma função que retorne uma tupla-3 (tripla) contendo o caractere fornecido com entrada, o mesmo caractere em letras 
minúsculas ou maiúsculas, e o seu número da tabela ASCII. Exemplo: -}
converte::Char->(Char,Char,Int)
converte   ch 
       | isLower ch = (ch,chr (charNum-32),charNum)
       | otherwise = (ch,chr (charNum+32),charNum)
       where charNum = ord ch

{- 2. Seja o cadastro de pessoas dado pela função a seguir: -}
pessoa rg 
        | rg == 1 = ("Joao Silva", 12, 'm')
        | rg == 2 = ("Jonas Souza", 51, 'm')
        | rg == 3 = ("Eliseu Miguel", 35, 'm')
        | rg == 4 = ("Adriana Avila", 25, 'f')
        | rg == 5 = ("Thiago Arruda", 34, 'm')
        | rg == 6 = ("Iago Carvalho", 30, 'm')
        | rg == 7 = ("Luiz Silva", 40, 'm')
        | rg == 8 = ("Ricardo Menezes", 38, 'm')
        | rg == 9 = ("Angela Moreno", 36, 'f')
        | rg == 10 = ("Jocileide Strauss" , 21, 'f')
        | otherwise = ("Nao ha ninguem mais", 9999, 'x')
{- Construa funções que retornem os seguintes dados: 
  a)O nome da pessoa de menor idade até um determinado registro. -}
getNome (nome,idade,sexo) = nome
getIdade (nome,idade,sexo) = idade
getSexo (nome,idade,sexo) = sexo

scanRG::(Int->Int->Bool)->Int->(String,Int,Char)->(String,Int,Char)
scanRG         f          lim  (nome,idade,sexo)
     | rgStat == 9999 = (nome,idade,sexo)
     | f (rgStat) idade = scanRG f (lim-1) (pessoa lim)
     | otherwise = scanRG f (lim-1) (nome,idade,sexo)
     where rgStat = getIdade (pessoa lim)

menorIdade::Int->(String,Int,Char)
menorIdade  lim = scanRG (<) lim (pessoa lim)

getNomeMenorIdade::Int->String
getNomeMenorIdade  lim = getNome (menorIdade lim)

--b) A idade média de todas as pessoas até um dado registro.
somaIdade::Float->Float
somaIdade  lim 
             | rgStat == 9999 = 0
             | otherwise = rgStat + somaIdade (lim-1) 
             where rgStat = getIdade (pessoa lim)

idadeMedia::Float->Float
idadeMedia  lim = (somaIdade lim)/lim

--c) O número de pessoas do sexo masculino.
quantidadeHomens::Int->Int
quantidadeHomens  lim
               | rgStat == 'x' = 0
               | rgStat == 'm' = 1 + quantidadeHomens (lim-1)
               | otherwise = quantidadeHomens (lim-1)
               where rgStat = getSexo (pessoa lim)

quantidadeHomensInterface::Int
quantidadeHomensInterface = quantidadeHomens 10

--d) O número do registro da pessoa de maior idade.
findRg::(String,Int,Char)->Int->Int
findRg           rg          num
     | getIdade (pessoa num) == 9999 = -1
     | pessoa num == rg = num
     | otherwise = findRg rg (num+1)

findRgInterface::(String,Int,Char)->Int 
findRgInterface           rg = findRg rg 1

maiorIdade::(String,Int,Char)
maiorIdade = scanRG (>) 10 (pessoa 10)

findRgMaiorIdade = findRgInterface (maiorIdade)

{- 3. Construa uma função em que, dado um caractere qualquer, retorne uma tupla3 com o caractere dado, o caractere dado na forma 
maiúscula/minúscula (o contrário do original) e o número ASCII do original. -}
analisaLetra::Char->(Char,Char,Int)
analisaLetra   ch = converte ch

{- 4. Construa uma função em Haskell que recebe 4 inteiros e devolve uma tupla-4 com os quatro valores originais, só que ordenados.
-}
-- geraLista::(Int,Int,Int,Int)->[Int]
-- geraLista  

-- ordena::Int->Int->Int->Int->(Int,Int,Int,Int)
-- ordena   a    b    c    d = ordenaTupla (a,b,c,d)

{- 5. Dadas duas datas (dI, mI, ar) e (d2, m2, a2), tal que data, ::; data-, construa uma função que retorne quantos dias existem 
entre estas duas datas, onde di define o dia do mês mj no ano ak.-}
-- distanciaDatas->(Int,Int,Int)->(Int,Int,Int)->Int
-- distanciaDatas   (d1,m1,a1)      (d2,m2,a2)
--              | 

{- 6. Crie uma função que receba os coeficientes de uma equação do segundo grau ax^2 + bx + c = O na forma (a, b, c) e retome as 
raízes desta equação. Trate o caso de raízes imaginárias, indicando um erro. -}
equacao::(Float,Float,Float)->(Float,Float)
equacao     (a,b,c)
      | delta < 0 = (0,0)
      | delta == 0 = (r1,r1)
      | otherwise = (r1,r2)
        where delta = (b*b) - 4*a*c
              r1 = (-b  + (sqrt delta))/2*a 
              r2 = (-b - (sqrt delta))/2*a

{- 7. Construa uma função que, dados três valores, verifique se os mesmos podem ser os lados de um triângulo. Se for possível 
formar o triângulo, retome uma tupla-2 com o tipo do triângulo formado (com relação às arestas) e o perímetro do mesmo. -}
isATriangle::(Int,Int,Int)->Bool
isATriangle     (a,b,c) = (a < b+c) && (b < a+c) && (c < a+b)

triangleType::(Int,Int,Int)->String
triangleType     (a,b,c)
           | (a == b) && (b == c) = "Equilatero"
           | (a == b) || (b == c) = "Isosceles"
           | otherwise = "Escaleno"

perimeter (a,b,c) = a+b+c

triangle::(Int,Int,Int)->(String,Int)
triangle     (a,b,c)
       | isATriangle (a,b,c) = (triangleType (a,b,c),perimeter (a,b,c))
       | otherwise = ("Nao e um triangulo",-1)