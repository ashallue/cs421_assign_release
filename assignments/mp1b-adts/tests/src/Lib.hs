--- Getting Started
--- ===============

--- Relevant Files
--- --------------

module Lib where


--- Problems
--- ========

--- Recursion
--- ---------

--- ### mytake

mytake :: Int -> [a] -> [a]
mytake n []     = []
mytake n (x:xs)
    | n <= 0    = []
    | otherwise = x : mytake (n-1) xs

--- ### mydrop

mydrop :: Int -> [a] -> [a]
mydrop n []     = []
mydrop n (x:xs)
    | n <= 0    = x : xs
    | otherwise = mydrop (n-1) xs

--- ### rev

rev :: [a] -> [a]
rev [] = []
rev xs = aux xs []
    where
        aux []     acc  = acc
        aux (x:xs) acc  = aux xs (x : acc)

--- ### app

app :: [a] -> [a] -> [a]
app []     ys = ys
app (x:xs) ys = x : app xs ys

--- ### inclist

inclist :: Num a => [a] -> [a]
inclist []     = []
inclist (x:xs) = x+1 : inclist xs

--- ### sumlist

sumlist :: Num a => [a] -> a
sumlist []     = 0
sumlist (x:xs) = x + sumlist xs

--- ### myzip

myzip :: [a] -> [b] -> [(a,b)]
myzip xs     []     = []
myzip []     ys     = []
myzip (x:xs) (y:ys) = (x,y) : myzip xs ys

--- ### addpairs

addpairs :: (Num a) => [a] -> [a] -> [a]
addpairs xs ys = addpairs' $ myzip xs ys
    where
        addpairs' []          = []
        addpairs' ((x,y):xys) = x+y : addpairs' xys

--- ### ones

ones :: [Integer]
ones = 1 : ones

--- ### nats

nats :: [Integer]
nats = [0..]

--- ### fib

fib :: [Integer]
fib = 0 : 1 : addpairs fib (tail fib)

--- Set Theory
--- ----------

--- ### add

add :: Ord a => a -> [a] -> [a]
add n []        = [n]
add n (x:xs)
    | n < x     = n : x : xs
    | n == x    = x : xs
    | otherwise = x : add n xs

--- ### union

union :: Ord a => [a] -> [a] -> [a]
union [] ys     = ys
union xs []     = xs
union (x:xs) (y:ys)
    | x == y    = x : union xs ys
    | x < y     = x : union xs (y:ys)
    | x > y     = y : union (x:xs) ys

--- ### intersect

intersect :: Ord a => [a] -> [a] -> [a]
intersect xs [] = []
intersect [] ys = []
intersect (x:xs) (y:ys)
    | x == y    = x : intersect xs ys
    | x < y     = intersect xs (y:ys)
    | otherwise = intersect (x:xs) ys

--- ### powerset

powerset :: Ord a => [a] -> [[a]]
powerset [] = [[]]
powerset (x:xs) = let pxs = powerset xs
                  in  pxs `union` (map (add x) pxs)

--- Higher Order Functions
--- ----------------------

--- ### inclist'

inclist' :: (Num a) => [a] -> [a]
inclist' xs = map (+1) xs

--- ### sumlist'

sumlist' :: (Num t) => [t] -> t
sumlist' xs = foldr (+) 0 xs

--- Algebraic Data Types
--- --------------------

data List a = Cons a (List a)
            | Nil
  deriving (Show, Eq)

data Exp = IntExp Integer
         | PlusExp [Exp]
         | MultExp [Exp]
  deriving (Show, Eq)

--- ### list2cons

list2cons :: [a] -> List a
list2cons []     = Nil
list2cons (x:xs) = Cons x (list2cons xs)

--- ### cons2list

cons2list :: List a -> [a]
cons2list Nil         = []
cons2list (Cons x xs) = x : cons2list xs

--- ### eval

eval :: Exp -> Integer
eval (IntExp n)   = n
eval (PlusExp is) = foldr (+) 0 $ map eval is
eval (MultExp is) = foldr (*) 1 $ map eval is

--- ### list2cons'

list2cons' :: [a] -> List a
list2cons' xs = foldr Cons Nil xs

--- ### BinTree

data BinTree a = Node a (BinTree a) (BinTree a)
               | Leaf
               deriving (Show)

--- ### sumTree

sumTree :: Num a => BinTree a -> a
sumTree Leaf         = 0
sumTree (Node i l r) = i + sumTree l + sumTree r

--- ### SimpVal

data SimpVal = IntVal Integer
             | BoolVal Bool
             | StrVal String
             | ExnVal String
             deriving (Show)

--- ### liftIntOp

liftIntOp :: (Integer -> Integer -> Integer) -> SimpVal -> SimpVal -> SimpVal
liftIntOp f (IntVal i1) (IntVal i2) = IntVal $ f i1 i2
liftIntOp _ _           _           = ExnVal "not an IntVal!"
