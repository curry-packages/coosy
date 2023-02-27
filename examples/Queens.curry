import Control.AllValues
import Observe

-- Place n queens on a chessboard so that no queen can capture another queen:
-- (this solution is due to Sergio Antoy)

queens :: [Int] -> [Int]
queens x | y =:= permute' x & null (allValues (capture x y)) = y  where y free

-- Observe the computation of permutations.
permute' :: [Int] -> [Int]
permute' = observeG (oList oLit ~> oList oLit) "permute" permute

permute :: [Int] -> [Int]
permute []     = []
permute (x:xs) | u ++ v =:= permute xs  = u ++ [x] ++ v
  where u,v free

capture :: [Int] -> [Int] -> Bool
capture x y = let l1,l2,l3,x1,x2,y1,y2 free in
  l1 ++ [(x1,y1)] ++ l2 ++ [(x2,y2)] ++ l3 =:= zip x y &
  (x1-y1 == x2-y2 || x1+y1 == x2+y2) =:= True


queens4 :: [Int]
queens4 = queens [1,2,3,4]

queens5 :: [Int]
queens5 = queens [1,2,3,4,5]

