{- Exercise 6, due (Nov 21, noon)

General exercise instructions:
- Exercises must be done *individually*.
- You may not import any Haskell libraries, unless explicitly told to.
- You may write helper functions freely; in fact, you are encouraged
  to do so to keep your code easy to understand.
- Your grade will be determined by our automated testing.
  You can find some sample tests on the course webpage.
- Submit early and often! MarkUs is rather slow when many people
  submit at once. It is your responsibility to make sure your work is
  submitted on time.
- No late submissions will be accepted!

This exercise is mainly a chance to practice writing code in Haskell,
although we'll take advantage of lazy evaluation to create some nice
infinite data structures. :)
-}

-- This line creates a module to allow exporting of functions.
-- DON'T CHANGE IT!
module Ex6 (primes, Tree(Empty, Node), treeSum,
            MaybeTree(EmptyM, NodeM), treeSumMaybe,
            MaybeInt(Success, Failure)) where

-- Question 1
-- Define 'primes', an infinite list of primes (in ascending order).
-- Remember that 1 is not a prime, but 2 is a prime.
-- You may want to use the definition of 'nats' found on page 89
-- in the course notes.

primes :: [Integer]
primes = filter is_prime nats

nats = 0 : map (\x -> x+1) nats
is_factor a b =(a `mod` b) == 0
potential_factors n = tail(tail(take(fromInteger n) nats))
is_prime :: Integer -> Bool
is_prime n=(n>1)&&(not(any(is_factor n) (potential_factors n)))


-- This is a datatype to define a binary tree. It follows the
-- recursive definition of a binary tree, which is:
--   - an empty tree
--   - a node containing a value with a left and right subtree
-- For this exercise, we'll stick with binary trees of integers.
-- In the second constructor, the first 'Tree' represents the left
-- subtree, and the second represents the right subtree.
data Tree = Empty | Node Integer Tree Tree deriving Show

-- An example of a tree.
tree :: Tree
tree = Node 5 (Node 3 Empty
                      (Node 3 Empty Empty))
              (Node 5 (Node 1 Empty Empty)
                      (Node (-10) Empty Empty))

-- Question 2
-- Define 'treeSum', which takes a tree and returns the sum of the
-- integers in the tree. Note that the empty tree has a sum of 0.


fplus :: Integer -> Integer -> Integer 
fplus 0 0 = 0
fplus a 0 = a
fplus 0 a = a
fplus a b = a + b

treeSum :: Tree -> Integer
treeSum Empty = 0
--treeSum (Node Nothing (Node Nothing Empty Empty) Empty) = 0
--treeSum (Node Nothing Empty (Node Nothing Empty Empty)) = 0
treeSum (Node node left right) = fplus node (fplus (treeSum left) (treeSum right))

-- This is a datatype that's similar to Tree, but contains MaybeInts
-- instead of integers.
data MaybeInt = Success Integer | Failure deriving Show
data MaybeTree = EmptyM | NodeM MaybeInt MaybeTree MaybeTree deriving Show

-- Question 3
-- Define 'treeSumMaybe', which takes a tree and returns the sum of the
-- "success" integers in the tree. Return 0 if the tree is empty,
-- or contains only failures.
treeSumMaybe :: MaybeTree -> Integer
treeSumMaybe = undefined

--fp :: Num a => Maybe a -> Maybe a -> Maybe a
--fp Nothing Nothing = Nothing
--fp (Just a) Nothing = Just a
--fp Nothing (Just a) = Just a
--fp (Just a) (Just b) = Just (a + b)

--treeSumMaybe Empty = Just 0
--treeSumMaybe (Node Nothing Empty Empty) = Nothing
--treeSumMaybe (Node Nothing (Node Nothing Empty Empty) Empty) = Nothing
--treeSumMaybe (Node Nothing Empty (Node Nothing Empty Empty)) = Nothing
--treeSumMaybe (Node node left right) = fp node (fp (treeSum left) (treeSum right))