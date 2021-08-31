list1 :: [Int]
list1 = [1,2,3,4,6,2,7,8,9]

list2 :: [Int]
list2 = [8,9]

list3 :: [Char]
list3 = "thiago"

list4 :: [Int]
list4 = [1,3,2,5,-4]

concatenacao :: [a] -> [a] -> [a] 
concatenacao [] x = x
concatenacao x [] = x
concatenacao (x:xs) y = x : concatenacao xs y
 
pertence :: Eq a => a -> [a] -> Bool
pertence _ [] = False
pertence a (x:xs) = (a == x) || pertence a xs

intersecao :: Eq a => [a] -> [a] -> [a]
intersecao x [] = []
intersecao [] x = []
intersecao (x:xs) (y:ys) =
  if pertence x (y:ys)
    then x : intersecao xs (y:ys)
    else intersecao xs (y:ys) 

inverso :: [a] -> [a]
inverso [] = []
inverso (x:xs) = concatenacao (inverso xs) [x]

primeiros :: Int -> [a] -> [a]
primeiros 0 _ = [] 
primeiros n [] = []
primeiros n (x:xs) = x : primeiros (n-1) xs

ultimos :: Int -> [a] -> [a]
ultimos 0 _ = []
ultimos n x = inverso $ primeiros n (inverso x)

binParaInt :: String -> Int
binParaInt [] = 0
binParaInt (x:xs) =
  if x == '0'
    then 0 + binParaInt xs
    else 2 ^ length xs + binParaInt xs

intParaBin :: Int -> String
intParaBin 0 = ""
intParaBin x = 
  if mod x 2 == 0
    then concatenacao (intParaBin (div x 2)) "0"
    else concatenacao (intParaBin (div x 2)) "1"

menorValor :: Ord a => [a] -> a
menorValor [x] = x
menorValor (x:y:xs) = 
  if y < x  
    then menorValor(y:xs)
    else menorValor(x:xs)

removerPrimeiro :: Eq a => [a] -> a -> [a]
removerPrimeiro [] _ = []
removerPrimeiro (x:xs) n = 
  if n == x 
    then xs
    else x : removerPrimeiro xs n 

ordenar :: Ord a => [a] -> [a]
ordenar [] = []
ordenar [x] = x : ordenar []
ordenar (x:xs) = menorValor (x:xs) : ordenar (removerPrimeiro (x:xs) (menorValor (x:xs)))

dobrar_dir :: (a -> b -> b) -> b -> [a] -> b
dobrar_dir f x [] = x
dobrar_dir f x (y:ys) = f y (dobrar_dir f x ys)

dobrar_esq :: (b -> a -> b) -> b -> [a] -> b
dobrar_esq f x [] = x
dobrar_esq f x (y:ys) = dobrar_esq f (f x y) ys 

filtrar :: (a -> Bool) -> [a] -> [a]
filtrar _ [] = []
filtrar f (x:xs) = 
  if f x  
    then x : filtrar f xs 
    else filtrar f xs 

impar :: Int  -> Bool 
impar n = mod n 2 == 1

impares :: [Int] -> [Int]
impares [] = []
impares n = filtrar impar n

mapear :: (a -> b) -> [a] -> [b]
mapear _ [] = []
mapear f (x:xs) =  f x : mapear f xs 

list5 :: [(Int,Int)]
list5 = [(1,5),(2,6),(3,7)]

primeiro :: (a,b) -> a
primeiro (a,b) = a

primeirosTupla :: [(a, b)] -> [a]
primeirosTupla [] = []
primeirosTupla n = mapear primeiro n

list6 :: [Bool]
list6 = [True, True, True]

list7 :: [Bool]
list7 = [True, False, True]

todos :: [Bool] -> Bool
todos [] = False
todos n = dobrar_dir (&&) True n

data Tree a = Leaf a
 | Branch (Tree a) (Tree a)
    deriving Show

arv :: Tree Int
arv = Branch (Branch (Branch (Leaf 7) (Branch (Leaf 72) (Leaf 1) ) ) (Leaf 2)) (Branch (Leaf 4) (Leaf 5))                                          

maior :: Ord a => Tree a -> a
maior (Leaf a) = a
maior (Branch e d) = 
  if (maior e) > (maior d) 
    then (maior e) 
    else (maior d)

altura :: Tree a -> Int 
altura (Leaf a) = 0
altura (Branch e d) = 
  1 + 
  if (altura e) > (altura d) 
    then (altura e) 
    else (altura d)
  
main :: IO()
main = do
 print $ concatenacao list1 list2
 print $ pertence 'g' list3
 print $ intersecao list2 list4
 print $ inverso list4 
 print $ primeiros 0 list2
 print $ ultimos 5 list1
 print $ binParaInt "111" 
 print $ intParaBin 42
 print $ menorValor list2
 print $ removerPrimeiro list1 2
 print $ ordenar list4
 print $ dobrar_dir (+) 0 list2 
 print $ dobrar_esq (*) 1 list2 
 print $ filtrar (==2) list4
 print $ impares list1 
 print $ mapear impar list1 
 print $ primeirosTupla list5
 print $ todos list6
 print $ todos list7
 print $ maior arv
 print $ altura arv 