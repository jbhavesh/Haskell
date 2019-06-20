sumsq :: Int -> Int
sumsq x = foldr (\x acc -> acc+x^2) 0 [1..x]

myMap :: (a->b)->[a]->[b]
myMap f xs = foldr(\x acc->(f x):acc) [] xs

myFilter :: (a->Bool)->[a]->[a]
myFilter f xs = foldr(\x acc->if (f x)==True then (x:acc) else acc) [] xs

myReverse :: [a]->[a]
myReverse xs = foldl(\acc x->x:acc) [] xs

remdups ::(Eq a)=> [a]->[a]
remdups xs = foldr(\x acc->if acc==[] then x:[] else (if (head acc)==x then acc else x:acc)) [] xs


inits :: [a]->[[a]]
inits=foldr f  [ [ ] ]
      where f x xss = []  :map(x:) xss
