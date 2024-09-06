module Lesson1 where

-- to explain about data structure
data Color = White | Gray | Black deriving (Eq, Show)

isItGray :: Color -> Bool
isItGray x
  | x == Gray = True
  | otherwise = False

data Review = Description String | Score Int deriving (Eq, Show)

howWasTheReview :: Review -> String
howWasTheReview (Description d) = d
howWasTheReview (Score s) = "The score was" ++ (show s)

data MyMaybe a = MyJust a | MyNothing deriving (Eq, Show)

printJust :: (Show a) => MyMaybe a -> String
printJust (MyJust x) = show x
printJust _ = "Nothing"

-- using Maybe and Either to detect error
safeHead1 :: [a] -> Maybe a
safeHead1 [] = Nothing
safeHead1 (x : xs) = Just x

safeHead2 :: [a] -> Either String a
safeHead2 [] = Left "The list is empty."
safeHead2 (x : xs) = Right x

-- recursive data structure
data List a = Nil | Cons a (List a) deriving (Eq, Show)

normalListToMyList :: [a] -> List a
normalListToMyList [] = Nil
normalListToMyList (x : xs) = Cons x (normalListToMyList xs)

data NaturalNumber = Zero | Succ NaturalNumber deriving (Eq, Show)

unSafeIntToNat :: Int -> NaturalNumber
unSafeIntToNat x
  | x == 0 = Zero
  | x > 0 = Succ $ unSafeIntToNat $ x - 1
  | otherwise = undefined

safeIntToNat :: Int -> Maybe NaturalNumber
safeIntToNat x
  | x == 0 = Just Zero
  | x > 0 = case safeIntToNat $ x - 1 of
      Nothing -> Nothing
      Just n -> Just $ Succ n
  | otherwise = Nothing

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Eq, Show)

singletonTree :: a -> Tree a
singletonTree x = Node x Empty Empty

insertTree :: (Ord a) => Tree a -> a -> Tree a
insertTree Empty x = singletonTree x
insertTree (Node a leftSub rightSub) x
  | x == a = Node a leftSub rightSub
  | x < a = Node a (insertTree leftSub x) rightSub
  | otherwise = Node a leftSub (insertTree rightSub x)

makeTreeFromList :: (Ord a) => [a] -> Tree a
makeTreeFromList = foldl insertTree Empty

unsafeFindElementFromTree :: (Ord a) => Tree a -> a -> String
unsafeFindElementFromTree Empty x = undefined
unsafeFindElementFromTree (Node a leftSub rightSub) x
  | x == a = ""
  | x < a = 'L' : (unsafeFindElementFromTree leftSub x)
  | otherwise = 'R' : (unsafeFindElementFromTree rightSub x)

safeFindElementFromTree :: (Ord a) => Tree a -> a -> Maybe String
safeFindElementFromTree Empty x = Nothing
safeFindElementFromTree (Node a leftSub rightSub) x
  | x == a = Just ""
  | x < a = case (safeFindElementFromTree leftSub x) of
      Nothing -> Nothing
      Just s -> Just $ 'L' : s
  | otherwise = case (safeFindElementFromTree rightSub x) of
      Nothing -> Nothing
      Just s -> Just $ 'R' : s

depthOfTree :: Tree a -> Int
depthOfTree Empty = 0
depthOfTree (Node _ leftSub rightSub) =
  if (depthOfTree leftSub) > (depthOfTree rightSub)
    then 1 + depthOfTree leftSub
    else 1 + depthOfTree rightSub

type SymbolValues = [(String, Bool)]

data Exp = Not Exp | And (Exp, Exp) | Or (Exp, Exp) | Symbol String deriving (Eq, Show)

unsafeEval :: SymbolValues -> Exp -> Bool
unsafeEval env (Symbol name) =
  let unsafeLookup :: String -> SymbolValues -> Bool
      unsafeLookup _ [] = undefined
      unsafeLookup name (x : xs) =
        if fst x == name
          then snd x
          else unsafeLookup name xs
   in unsafeLookup name env
unsafeEval env (Not exp) = not (unsafeEval env exp)
unsafeEval env (And (e1, e2)) = (unsafeEval env e1) && (unsafeEval env e2)
unsafeEval env (Or (e1, e2)) = (unsafeEval env e1) && (unsafeEval env e2)

safeEval :: [(String, Bool)] -> Exp -> Maybe Bool
safeEval env (Symbol name) = lookup name env
safeEval env (Not exp) = case safeEval env exp of
  Nothing -> Nothing
  Just v -> Just $ not v
safeEval env (And (e1, e2)) = case safeEval env e1 of
  Nothing -> Nothing
  Just v1 -> case safeEval env e2 of
    Nothing -> Nothing
    Just v2 -> Just $ v1 && v2
safeEval env (Or (e1, e2)) = case safeEval env e1 of
  Nothing -> Nothing
  Just v1 -> case safeEval env e2 of
    Nothing -> Nothing
    Just v2 -> Just $ v1 || v2

symbols :: SymbolValues
symbols = [("x", True), ("y", False), ("z", True)]

exp1 :: Exp
exp1 = And ((Or (Symbol "x", And (Not (Symbol "y"), Symbol "z"))), Symbol "y")
