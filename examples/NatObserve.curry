import Observe

data Nat = O | S Nat

coin :: Nat
coin = O
coin = S O

plus :: Nat -> Nat -> Nat
plus O x     = x
plus (S x) y = S (plus x y)

-- To observe Nat values, click the `Add observers` button and select
-- this program.

-- Then you can observe plus by:

-- main = observeG (oNat ~~> oNat ~~> oNat) " + " plus O coin

