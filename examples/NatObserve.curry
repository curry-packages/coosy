import Observe

data Nat = O | S Nat

coin = O
coin = S O

plus O x     = x
plus (S x) y = S (plus x y)

-- To observe Nat values, click the `Add observers` button and select
-- this program.

-- Then you can observe plus by:

-- main = observe (oNat ~> oNat ~> oNat) " + " plus O coin

