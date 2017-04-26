import Observe

-- Compute the last element in a list using append:

append :: [a] -> [a] -> [a]
append []     ys = ys
append (x:xs) ys = x : append xs ys


last :: [a] -> a
last xs | append ys [x] =:= xs
        = x  where x,ys free

-- We observe the usage of the function `last` on integer lists:
last' :: [Int] -> Int
last' = observe (oList oInt ~> oInt) "last" last

main1 = last' [1,2,3,4]

main2 n = last' (take n (repeat x))  where x free

-- Important: compute only first solution by `:set +first`
main3 = last' l =:= 42 & length l =:= 5 where l free

