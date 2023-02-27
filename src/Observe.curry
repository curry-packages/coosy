------------------------------------------------------------------------------
--- The `Oberserve` library containing combinators to observe data.
---
--- @author Bernd Brassle, Olaf Chitil, Michael Hanus, Frank Huch
--- @version February 2023
------------------------------------------------------------------------------

module Observe
  (observe, observeG,
   oLit, oInt, oBool, oChar, oFloat,
   oOpaque,
   oOpaqueConstr,
   oList,
   oString,
   oPair, oTriple, o4Tuple, o5Tuple,
   oMaybe,
   oEither,
   oFun, (~>), oFunFG, (~~>), oFunG, (~~~>),
   o0,o1,o2,o3,o4,o5,
   clearLogFile,
   Observer,
   derive
  )
 where

import System.IO.Unsafe ( isVar, spawnConstraint, unsafePerformIO )

import Data.Global      ( GlobalT, globalT, readGlobalT, writeGlobalT )
import System.Process   ( system )

import Coosy.Derive     ( derive )
import Coosy.Trace


infixr 5 ~>, ~~>, ~~~>

type Observer a = (a -> Label -> EventID -> [EventID] -> a)

------------------------------------------------------------------------------
--- The basic operation to observe the evaluation of data structures.
--- It has a `Data` context so that it can also observe the instantiation
--- of free variables occurring in data structures.
observe :: Data a => Observer a -> String -> a -> a
observe observeA label x = initialObserver observeA 0 x label (-1) preds
 where preds free

initialObserver :: Data a => Observer a -> Int -> Observer a 
initialObserver observeA argNr x label parent preds = unsafePerformIO $ do
  clearFileCheck
  observerIO observeA argNr x label parent preds

observer :: Data a => Observer a -> Int -> Observer a
observer observeA argNr x label parent preds = unsafePerformIO $
  observerIO observeA argNr x label parent preds

observerIO :: Data a =>
              Observer a -> Int -> a -> Label -> EventID -> [EventID] -> IO a
observerIO observeA argNr x l parent preds = do
  (eventID, newPreds) <- recordEvent l (Demand argNr) parent preds
  if isVar x 
    then 
      do (logVarID,logVarPreds) <- recordEvent l LogVar eventID newPreds
         spawnConstraint 
           (seq (ensureNotFree x) (observeA x l logVarID logVarPreds =:= x))
           (return x)
    else return $ observeA x l eventID newPreds


--- The basic operation to observe the evaluation of ground data structures.
--- It does not require a `Data` context but it can not observe
--- the instantiation of free variables occurring in data structures.
--- Thus, it has to be ensured that free variables do not occur in the
--- observed structures, otherwise this observer always suspends.
observeG :: Observer a -> String -> a -> a
observeG observeA label x =
  initialObserverG observeA 0 x label (-1) preds
 where preds free

initialObserverG :: Observer a -> Int -> Observer a
initialObserverG observeA argNr x label parent preds = unsafePerformIO $ do
  clearFileCheck
  observerGIO observeA argNr x label parent preds

observerG :: Observer a -> Int -> Observer a
observerG observeA argNr x label parent preds = unsafePerformIO $
  observerGIO observeA argNr x label parent preds

observerGIO ::
             Observer a -> Int -> a -> Label -> EventID -> [EventID] -> IO a
observerGIO observeA argNr x l parent preds = do
  (eventID, newPreds) <- recordEvent l (Demand argNr) parent preds
  (ensureNotFree x) `seq` return (observeA x l eventID newPreds)

------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- Combinators to construct observers for various kinds of data.

-- An observer for literals.
oLit :: Show a => Observer a
oLit l = o0 (show l) l

-- An observer for integers.
oInt :: Observer Int 
oInt = oLit

-- An observer for Booleans.
oBool :: Observer Bool
oBool = oLit

-- An observer for chacacters.
oChar :: Observer Char
oChar = oLit

-- An observer for floats.
oFloat :: Observer Float 
oFloat = oLit

-- An opaque observer which does not observe the values.
-- This might be useful to observe polymorphic types.
oOpaque :: Observer _
oOpaque x = o0 "#" x

oOpaqueConstr :: String -> Observer _
oOpaqueConstr constr x = o0 constr x

-- An observer combinator for lists.
oList :: Data a => Observer a -> Observer [a]
oList _        []     = o0 "[]" []
oList observeA (x:xs) = o2 observeA (oList observeA) "(:)" (:) x xs

-- An observer for strings.
oString :: Observer String
oString = oList oChar

-- An observer combinator for pairs.
oPair  :: (Data a, Data b) => Observer a -> Observer b -> Observer (a,b)
oPair observeA observeB (x,y) =
  o2 observeA observeB "(,)" (\a b -> (a,b)) x y

-- An observer combinator for triples.
oTriple :: (Data a, Data b, Data c) =>
           Observer a -> Observer b -> Observer c -> Observer (a,b,c)
oTriple observeA observeB observeC (x,y,z) =
  o3 observeA observeB observeC "(,,)" (\a b c -> (a,b,c)) x y z

-- An observer combinator for quadruples.
o4Tuple :: (Data a, Data b, Data c, Data d) =>
           Observer a -> Observer b -> Observer c -> Observer d
                                                  -> Observer (a,b,c,d)
o4Tuple observeA observeB observeC observeD (x1,x2,x3,x4) =
  o4 observeA observeB observeC observeD "(,,,)"
     (\a b c d -> (a,b,c,d)) x1 x2 x3 x4

-- An observer combinator for 5-tuples.
o5Tuple :: (Data a, Data b, Data c, Data d, Data e) =>
           Observer a -> Observer b -> Observer c -> Observer d -> Observer e
                                                  -> Observer (a,b,c,d,e)
o5Tuple observeA observeB observeC observeD observeE (x1,x2,x3,x4,x5) =
  o5 observeA observeB observeC observeD observeE "(,,,,)"
     (\a b c d e -> (a,b,c,d,e)) x1 x2 x3 x4 x5

-- An observer combinator for the `Maybe` type.
oMaybe :: Data a => Observer a -> Observer (Maybe a)
oMaybe _        Nothing  = o0 "Nothing" Nothing
oMaybe observeA (Just x) = o1 observeA "Just" Just x

-- An observer combinator for the `Either` type.
oEither :: (Data a, Data b) => Observer a -> Observer b -> Observer (Either a b)
oEither observeA _ (Left a)  = o1 observeA "Left" Left a
oEither _ observeB (Right b) = o1 observeB "Right" Right b

-- An observer combinator for the `IO` type.
oIO :: Data a => Observer a -> Observer (IO a)
oIO observeA action parent preds label = 
       action >>= \res -> o1 observeA "<IO>" return res parent preds label

-- Construct an observer for functions where the argument and result might be
-- a free variable.
oFun :: (Data a, Data b) => Observer a -> Observer b -> Observer (a -> b)
oFun observeA observeB f label parent preds arg =
  (unsafePerformIO $ do
    (eventID,newPreds) <- recordEvent label Fun parent preds
    return (\x -> (observer observeB 2
                     (f (observer observeA 1 x label eventID newPreds))
                     label eventID newPreds)))
  arg
      
-- Construct an observer for functions where the argument and result might be
-- a free variable.
(~>) :: (Data a, Data b) => Observer a -> Observer b -> Observer (a -> b)
a ~> b = oFun a b

-- Construct an observer for functions where the argument might be
-- a free variable and the result is always non-free (e.g., a functional value).
oFunFG :: Data a => Observer a -> Observer b -> Observer (a -> b)
oFunFG observeA observeB f label parent preds arg =
  (unsafePerformIO $ do
    (eventID,newPreds) <- recordEvent label Fun parent preds
    return (\x -> observerG observeB 2
                    (f (observer observeA 1 x label eventID newPreds))
                    label eventID newPreds))
  arg

-- Construct an observer for functions where the argument might be
-- a free variable and the result is always non-free (e.g., a functional value).
(~~>) :: Data a => Observer a -> Observer b -> Observer (a -> b)
a ~~> b = oFunFG a b

-- Construct an observer for functions where the argument and the result
-- are never free variables.
oFunG :: Observer a -> Observer b -> Observer (a -> b)
oFunG observeA observeB f label parent preds arg =
  (unsafePerformIO $ do
    (eventID,newPreds) <- recordEvent label Fun parent preds
    return (\x -> observerG observeB 2
                    (f (observerG observeA 1 x label eventID newPreds))
                    label eventID newPreds))
  arg

-- Construct an observer for functions where the argument and the result
-- are never free variables.
(~~~>) :: Observer a -> Observer b -> Observer (a -> b)
a ~~~> b = oFunG a b

------------------------------------------------------------------------------
-- Observers for constructors.

-- An observer combinator for 0-ary constructors.
o0 :: String -> Observer _
o0 constrStr x label parent preds = unsafePerformIO $ do
  recordEvent label (Value 0 constrStr) parent preds
  return x

-- An observer combinator for unary constructors.
o1 :: Data a => Observer a -> String
   -> (a -> b) -> a -> Label -> EventID -> [EventID] -> b
o1 observeA constrStr constr x label parent preds = unsafePerformIO $ do
  (eventID,newPreds) <- recordEvent label (Value 1 constrStr) parent preds
  return (constr (observer observeA 1 x label eventID newPreds))
   
-- An observer combinator for binary constructors.
o2 :: (Data a, Data b) => Observer a -> Observer b -> String ->
      (a -> b -> c) -> a -> b -> Label -> EventID -> [EventID] ->  c
o2 observeA observeB constrStr constr x1 x2 label parent preds =
  unsafePerformIO $ do
    (eventID,newPreds) <- recordEvent label (Value 2 constrStr) parent preds
    return (constr (observer observeA 1 x1 label eventID newPreds)
                   (observer observeB 2 x2 label eventID newPreds))
    
-- An observer combinator for ternary constructors.
o3 :: (Data a, Data b, Data c) =>
      Observer a -> Observer b -> Observer c -> String ->
      (a -> b -> c -> d) -> 
      a -> b -> c -> Label -> EventID -> [EventID] -> d
o3 observeA observeB observeC constrStr constr x1 x2 x3 label parent preds =
  unsafePerformIO $ do
    (eventID,newPreds) <- recordEvent label (Value 3 constrStr) parent preds
    return (constr (observer observeA 1 x1 label eventID newPreds)
                   (observer observeB 2 x2 label eventID newPreds)
                   (observer observeC 3 x3 label eventID newPreds))

-- An observer combinator for constructors of arity 4.
o4 :: (Data a, Data b, Data c, Data d) =>
      Observer a -> Observer b -> Observer c -> Observer d -> String ->
      (a -> b -> c -> d -> e) -> 
      a -> b -> c -> d -> Label -> EventID -> [EventID] -> e
o4 observeA observeB observeC observeD constrStr constr x1 x2 x3 x4
  label parent preds =
  unsafePerformIO $ do
    (eventID,newPreds) <- recordEvent label (Value 4 constrStr) parent preds
    return (constr (observer observeA 1 x1 label eventID newPreds)
                   (observer observeB 2 x2 label eventID newPreds)
                   (observer observeC 3 x3 label eventID newPreds)
                   (observer observeD 4 x4 label eventID newPreds))

-- An observer combinator for constructors of arity 5.
o5 :: (Data a, Data b, Data c, Data d, Data e) =>
      Observer a -> Observer b -> Observer c -> Observer d ->
      Observer e -> String ->
      (a -> b -> c -> d -> e -> f) ->
      a -> b -> c -> d -> e -> Label -> EventID -> [EventID] -> f
o5 observeA observeB observeC observeD observeE constrStr constr
  x1 x2 x3 x4 x5 label parent preds =
  unsafePerformIO $ do
    (eventID,newPreds) <- recordEvent label (Value 5 constrStr) parent preds
    return (constr (observer observeA 1 x1 label eventID newPreds)
                   (observer observeB 2 x2 label eventID newPreds)
                   (observer observeC 3 x3 label eventID newPreds)
                   (observer observeD 4 x4 label eventID newPreds)
                   (observer observeE 5 x5 label eventID newPreds))

------------------------------------------------------------------------------
-- Auxiliary definitions.

globalEventID :: GlobalT EventID
globalEventID = globalT "Mod.gt" 0

getNewID :: IO EventID
getNewID = do
   n <- readGlobalT globalEventID
   writeGlobalT globalEventID (n+1)
   return n

getPred :: EventID -> [EventID] -> (EventID,[EventID])
getPred p xs | isVar xs  = (p,xs)
             | otherwise = getPred (head xs) (tail xs)

writeToTraceFile :: Label -> Event -> IO ()
writeToTraceFile label event =
  appendFile (logFile label) (showEvent event ++ "\n")

recordEvent :: Label -> (EventID -> EventID -> EventID -> Event) ->
               EventID -> [EventID] -> IO (EventID,[EventID])
recordEvent label event parent preds = do
  eventID <- getNewID
  let (pred,logVar) = getPred parent preds 
  doSolve (logVar =:= (eventID:newLogVar))
  writeToTraceFile label (event eventID parent pred)
  return (eventID, newLogVar)
 where newLogVar free

clearLogFile :: IO ()
clearLogFile = do
   system $ "touch " ++ logFile "" ++ "; rm " ++ logFile "*"
   writeGlobalT globalEventID 0

clearFileCheck :: IO ()
clearFileCheck =
  let clearFile = logFileClear in
   readFile clearFile >>= \clearStr ->
   if clearStr == "1"
     then do writeGlobalT globalEventID 0
             writeFile clearFile ""
     else return ()

------------------------------------------------------------------------------
