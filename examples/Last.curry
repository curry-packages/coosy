import Observe

-- Compute the last element in a list using append:

append :: [a] -> [a] -> [a]
append []     ys = ys
append (x:xs) ys = x : append xs ys


last :: Data a => [a] -> a
last xs | append ys [x] =:= xs
        = x  where x,ys free

-- We observe the usage of the function `last` on integer lists.
-- Note that we still observe the evaluation of free variables
-- occurring in the arguments.
last' :: [Int] -> Int
last' = observeG (oList oInt ~> oInt) "last" last

main1 :: Int
main1 = last' [1,2,3,4]

-- Observe a list of free variables.
main2 :: Int -> Int
main2 n = last' (take n (repeat x))  where x free

-- Important: compute only first solution by `:set +first`
main3 :: Bool
main3 = last' xs =:= 42 & length xs =:= 5 where xs free
