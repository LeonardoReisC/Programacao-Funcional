{- 1. Using a list comprehension, give an expression that calculates the sum 1^2 + 2^2 + ... 100^2 of the first one hundred integer 
squares. -}

getExpression::Int->[Int]
getExpression   n = [ x^2 | x <- [1..n] ]

getResult::Int->Int
getResult   n = foldr1 (+) (getExpression n) 

getResult100 = getResult 100

{- 2. In a similar way to the function length, show how the library function replicate :: Int -> a -> [a] that produces a list of 
identical elements can be defined using a list comprehension. -}

rep::Int->t->[t]
rep   n   a = [ a | n <- [1..n]]

{- 3. A triple (x, y, z) of positive integers is pythagorean if x^2 + y^2 = z^2. Using a list comprehension, define a function 
pyths :: Int -> [(Int, Int, Int)] that returns the list of all pythagorean triples whose components are at most a given limit. -}

isPythagorean::Int->Int->Int->Bool
isPythagorean   a    b    c = ((a^2 + b^2) == c^2)

pyths::Int->[(Int,Int,Int)]
pyths   n = [ (x,y,z) | x <- [1..n], y <- [1..n], z <- [1..n], isPythagorean x y z]

{- 4. A positive integer is perfect if it equals the sum of its factors, excluding the number itself. Using a list comprehension 
and the function factors, define a function perfects :: Int -> [Int] that returns the list of all perfect numbers up to a given 
limit. -}

factors::Int->[Int]
factors  num = [ fac | fac <- [1..(num)], ((mod num fac) == 0)]

perfects::Int->[Int]
perfects  num = [ pNum | pNum <- [1..num], ((foldr1 (+) (factors pNum)) == 2*pNum)]

{- 5. Show how the single comprehension [(x,y) | x <- [1,2,3], y <- [4,5,6]] with two generators can be re-expressed using two 
comprehensions with single generators. Hint: make use of the library function concat and nest one comprehension within the other. -}

alternative::[Int]->[Int]->[[(Int,Int)]]
alternative    l1    l2  = [[(x,y)| y <- l2] | x <- l1]

alternativeConcat::[Int]->[Int]->[(Int,Int)]
alternativeConcat    l1     l2  = concat (alternative l1 l2)

{- 6. Define the function find used in the function positions. -}

find:: Eq t => t->[(t,Int)]->[Int]
find   x     l   = [ snd d | d <- l, x == (fst d) ]

positions::Eq t=> t->[t]->[Int]
positions         x   xs = find x (zip xs [0..n])
        where n = (length xs) - 1

{- 7. The scalar product of two lists of integers xs and ys of length n is given by the sum of the products of corresponding 
integers. -}

scalarProduct::[Int]->[Int]->Int
scalarProduct    xs     ys = foldr1 (+) [ (fst d) * (snd d) | d <- zip xs ys]

{- 8. Define the exponentiation operator &! for non-negative integers using the same pattern of recursion as the multiplication 
operator *, and show how 2 &! 3 is evaluated using your definition. -}

(&!)::Int->Int->Int
(&!)   a    0 =  1
(&!)   a    e = a * (&!) a (e-1)

{- 10. 4. Define a function dec2int :: [Int] -> Int that converts a decimal number into an integer. -}

dec2Int::[Int]->Int
dec2Int   dec = scalarProduct (reverse dec) [ 10^u | u <- [0..(length dec)]]