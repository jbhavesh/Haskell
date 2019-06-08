prefix :: String->String->Bool
prefix [] ys = True
prefix (x:xs) [] = False
prefix (x:xs) (y:ys) = (x==y) && (prefix xs ys)

substring :: String->String->Bool
substring xs [] = False
substring xs ys
              |prefix xs ys = True
              |substring xs (tail ys) = True
              |otherwise = False

sort' :: [String]->[String]
sort' (x:y:xs)  
           | xs == [] = if(length x >= length y) then (x:y:[]) else (y:x:[])    
           | length x >= length y = (x:sort'(y:xs))
           | length x < length y  = (y:sort'(x:xs))
          

sublist :: String->[String]->[String]
sublist x (ys)= sort' ([y | y<-ys, substring x y == True])



subsets :: [Int]->[[Int]]
subsets [] = [[]]
subsets (x:xs) =subsets xs++[x:y | y<-subsets xs] 


isprime :: Int->Bool
isprime x = not (0 `elem` [x `mod` i|i<-[2..(x `div` 2)]])

pprime :: [Int]
pprime = [2]++[xs|xs<-[3,5..],isprime xs == True]

fibb :: Int->Int
fibb 0=0
fibb 1=1
fibb x= fibb(x-1) + fibb(x-2)

infi_fib :: [Int]
infi_fib = [fibb x|x<-[1..]]

string2int x = read x + 0
