-- Some examples to demonstrate the usage of COOSy:

import Observe

-- Observe the evaluation of an integer expression (x+y):
ex1 :: Int
ex1 = let x = 2
          y = 5
      in (observe oInt "Addition" (x+y))


-- Observe the evaluation of an infinite list (from 1):
ex2 :: [Int]
ex2 = take 10 (observe (oList oInt) "enumFrom 1" [1..])


-- Observe the evaluation (i.e., instantiations) of a logical variable x:
ex3 :: Bool
ex3 = let x,y free in (observe (oList oInt) "List variable" x) ++ y =:= [1,2]


-- Observe the evaluation of a function (reverse):
ex4 :: [Int]
ex4 = (observeG (oList oInt ~> oList oInt) "reverse" reverse) [1,2,3]


-- Observe all evaluations of a function (reverse):
orev :: [Int] -> [Int]
orev = observeG (oList oInt ~> oList oInt) "all_reverse" reverse

ex5 :: [Int]
ex5 = orev [1,2] ++ orev [3,4]


-- Observe all evaluations of a higher-order function (foldr):
ex6 :: Int
ex6 = (observeG ((oInt ~~> oInt ~~> oInt) ~~~> oInt ~~> oList oInt ~~> oInt)
                "foldr+" foldr) (+) 0 [1..4]

-- Observe all evaluations of a higher-order function (foldr):
ex7 :: Int
ex7 = (observeG ((oInt ~~> oInt ~~> oInt) ~~~> oInt ~~> oList oInt ~~> oInt)
                "foldr*" foldr) (*) 0 [1..10]

-- Observe the evaluation of a string.
ex8 :: String
ex8 = observe oString "ex7" ((chr 129):"k\fj\bh\rdf\n\t\\\"")

-- Observe the evaluation of a list while calculating its length:
ex9 :: Int
ex9 = length (observe (oList oInt) "length" [1..100])

-- Nondeterminism
ex10 :: Int
ex10 = let x = observeG (oInt ~> oInt) "coin" coin 1
       in x + x

coin :: Int -> Int
coin x | x==x = 0
coin x | x==x = 1

-- Nondeterminism over functional values
coinF :: Int -> Int
coinF = f
coinF = g

f :: Int -> Int
f 1 = 1

g :: Int -> Int
g 1 = 2

ex11 :: Int
ex11 = let h = (observeG (oInt ~> oInt) "coinF" coinF)
       in h 1 + h 1
