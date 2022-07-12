-- Aula para suprir o feriado do dia 16-06-2022

type Dia = Int
type VendaR = Int

-- define o período de recursão
periodo::Int
periodo = 7

-- tabela de vendas
vendas :: Int -> Int
vendas 1 = 41
vendas 2 = 72
vendas 3 = 41
vendas 4 = 2
vendas 5 = 91
vendas 6 = 55
vendas 7 = 41
vendas _ = 0

vendaRelevante2 :: (Int->Int->Bool) -> Dia -> VendaR -> Int
vendaRelevante2  _  0  vendaR = vendaR
vendaRelevante2 f dia vendaR 
   | f vendaR (vendas dia)  = vendaRelevante2 f (dia-1) vendaR
   | otherwise = vendaRelevante2 f (dia-1) (vendas dia) 

vendaRelevante :: (Int->Int->Int) -> Dia -> VendaR -> Int
vendaRelevante _  0  vendaR = vendaR
vendaRelevante f dia vendaR = vendaRelevante f (dia-1) (f vendaR (vendas dia))

-- função interface usuario para vendaRelevante 
vendaRmm :: (Int->Int->Int)->Int
vendaRmm f = vendaRelevante f periodo (vendas periodo)

-- função auxiliar maior
maior::Int->Int->Int
maior a b
  | (a>b) = a
  | otherwise = b
  
-- função auxiliar menor
menor::Int->Int->Int
menor a b
  | (a<b) = a
  | otherwise = b  
  

-- retorna maior venda, mas depende de parâmetro
maiorv::Int->Int
maiorv 0   = vendas 0
maiorv dia = maior (vendas dia) (maiorv (dia-1))

-- avançado (apenas teste)
maiorMenorv::(Int->Int->Int)->Int->Int
maiorMenorv  _ 0 = vendas 0
maiorMenorv  f dia = f (vendas dia) (maiorv (dia-1))

-- retorna maior venda
maiorvenda = maiorv periodo

-- retorna o dia de certa venda, mas depende de parâmetro

diav :: Int -> Int -> Int
diav    (-1) _ = (-1)
diav    i  v
  | (vendas i) == v = i
  | otherwise = diav (i-1) v

-- dia de certa venda
diavenda::Int->Int
diavenda x = diav periodo x

diamaiorvenda = diavenda maiorvenda
  
  
-- Resolva as questões usando o Hugs:
-- 1) Implemente uma função que retorna a lista com os valores das vendas durante o período
-- Ex: para a função vendas atual, sua função deverá retornar [41, 55, 91, 2, 41, 72, 41]
listaVendas::(Int->Int)->[Int]
listaVendas vendas 1 = vendas 1
listaVendas f = f ++ listaVendas vendas (periodo-1)

-- 2) Implemente uma função que retorna uma lista com os dias que venderam uma quantidade definida
-- Ex: se a função receber 41, deverá retornar [7, 3, 1]


-- 3) Implemente uma função que retorne uma lista de dias que apresentaram vendas superiores a um valor
-- Ex: se a função receber 50, deverá retornar [6, 5, 2]




