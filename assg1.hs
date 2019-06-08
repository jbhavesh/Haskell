combinations :: Int->[a]->[[a]]
combinations 0 _ = [[]]
combinations k xs = [xs !! i : x|i<-[0..(length xs -1)],x<-combinations (k-1) (drop (i+1) xs)]

compress :: (Eq a) => [a]->[a]
compress [] = []
compress (x:y:xs)
    |x==y = x:compress xs
    |otherwise = x:y:compress xs


dupli :: [a]->[a]
dupli [] = []
dupli (x:xs) = x:x:dupli (xs)


insertAt :: a->[a]->Int->[a]
insertAt x xs y = take (y-1) xs ++ [x] ++ drop (length xs - y -1) xs

myLast :: [a]->a
myLast [x]=x
myLast (_:xs)=myLast(xs)

myReverse :: [a]->[a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

isPallindrome :: (Eq a)=>[a]->Bool
isPallindrome x
       |x == reverse x = True
       |otherwise = False

isprime :: Int->Bool
isprime x = not (0 `elem` [x `mod` i|i<-[2..(x `div` 2)]])

rotate a b = (drop a b) ++ (take b a)
