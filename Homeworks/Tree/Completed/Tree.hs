-- This is a regular BST, what we have done in Lesson 1.
-- It contains Tree data structure, and a constructor.
-- And we've just learned about the Functor typeclass.
-- So what we're going to do is,
-- We'll implement the Functor typeclass for Tree type.

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Eq, Show)

singletonTree :: a -> Tree a
singletonTree x = Node x Empty Empty

insertTree :: (Ord a) => a -> Tree a -> Tree a
insertTree x Empty = singletonTree x
insertTree x (Node a leftSub rightSub)
    | x == a = Node a leftSub rightSub
    | x < a = Node a (insertTree x leftSub) rightSub
    | otherwise = Node a leftSub (insertTree x rightSub)

makeTreeFromList :: (Ord a) => [a] -> Tree a
makeTreeFromList = foldl (flip insertTree) Empty

-- As we learned before, Functor typeclass is used for
-- "Lifting over" the type, which means that
-- We're drilling the type wrapper with the regular function,
-- which is not designed for the type wrapper to apply it inside the type wrapper.
-- For example, fmap (+3) (Just 3) evaluates to (Just 6), and fmap (+3) [1,2,3] evaluates to [4,5,6].

-- Functor typeclass includes only one function, fmap.
-- Just implement this for Tree, in the following code.

instance Functor Tree where
    fmap :: (a -> b) -> Tree a -> Tree b
    fmap _ Empty = Empty
    fmap f (Node n l r) = Node (f n) (fmap f l) (fmap f r)
    
-- If the following expression evaluates to True,
-- you have implemented it right, maybe.

test :: Bool
test = fmap (+3) (Node 3 (Node 2 Empty Empty) (Node 5 (Node 4 Empty Empty) (Node 6 Empty Empty))) == (Node 6 (Node 5 Empty Empty) (Node 8 (Node 7 Empty Empty) (Node 9 Empty Empty)))
    