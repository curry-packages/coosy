import Observe

data Tree etype ntype = Leaf etype | Node ntype [Tree etype ntype]

-- To observe Tree values, click the `Add observers` button and select
-- this program.
