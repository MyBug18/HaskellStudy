module Lesson3 where

import Lesson1

-- What is Monad?
class (Applicative m) => MyMonad m where
  (>>=.) :: m a -> (a -> m b) -> m b

  (>>.) :: m a -> m b -> m b
  a >>. b = a >>=. (\_ -> b)

  myReturn :: a -> m a
  {-# MINIMAL (>>=.), myReturn #-}

instance MyMonad Maybe where
  (>>=.) :: Maybe a -> (a -> Maybe b) -> Maybe b
  (Just x) >>=. f = f x
  _ >>=. _ = Nothing

  myReturn :: a -> Maybe a
  myReturn x = Just x

instance MyMonad (Either a) where
  (>>=.) :: Either a1 a2 -> (a2 -> Either a1 b) -> Either a1 b
  (Right x) >>=. f = f x
  (Left x) >>=. _ = Left x

  myReturn :: a2 -> Either a1 a2
  myReturn x = Right x

instance MyMonad [] where
  (>>=.) :: [a] -> (a -> [b]) -> [b]
  (x : xs) >>=. f = (f x) ++ (xs >>=. f)
  _ >>=. _ = []

  myReturn :: a -> [a]
  myReturn x = [x]

badAdd2Maybe :: (Num a) => Maybe a -> Maybe a -> Maybe a
badAdd2Maybe x y =
  case x of
    Just n1 ->
      case y of
        Just n2 -> Just (n1 + n2)
        Nothing -> Nothing
    Nothing -> Nothing

goodAdd2Maybe :: (Num a) => Maybe a -> Maybe a -> Maybe a
goodAdd2Maybe x y =
  x >>= \n1 ->
    y >>= \n2 ->
      return $ n1 + n2

moreGoodAdd2Maybe :: (Num a) => Maybe a -> Maybe a -> Maybe a
moreGoodAdd2Maybe x y = do
  n1 <- x
  n2 <- y
  return $ n1 + n2

-- implement safe eval function with Monad with do notation
eval :: SymbolValues -> Exp -> Maybe Bool
eval env (Symbol name) = lookup name env
eval env (Not exp) =
  let result = (eval env exp)
   in if result == Nothing then Nothing else fmap not result
eval env (And (e1, e2)) = do
  x <- eval env e1
  y <- eval env e2
  return $ x && y
eval env (Or (e1, e2)) = do
  x <- eval env e1
  y <- eval env e2
  return $ x || y

type Clown = (Int, Int)

data Side = L | R deriving (Eq, Show)

begin :: Either String Clown
begin = return (0, 0)

isDangerous :: Clown -> Bool
isDangerous (l, r) = if abs (l - r) > 3 then True else False

birdsLand :: Side -> Int -> Clown -> Either String Clown
birdsLand side n (l, r) =
  let clown = if side == L then (l + n, r) else (l, r + n)
   in if isDangerous clown then Left "Balance breaked!" else Right clown

banana :: Clown -> Either String Clown
banana _ = Left "Stepped on banana!"

failed :: Either String Clown
failed = begin >>= birdsLand L 1 >>= birdsLand R 7

successful :: Either String Clown
successful = begin >>= birdsLand L 3 >>= birdsLand R 3 >>= birdsLand L 2

slipped :: Either String Clown
slipped = successful >>= banana

type Routine = [Clown -> Either String Clown]

doRoutine :: Routine -> Either String Clown
doRoutine routine = foldl (>>=) begin routine

routine1 :: Routine
routine1 = [birdsLand L 3, birdsLand R 5, birdsLand L 5]

routine2 :: Routine
routine2 = [birdsLand L 3, birdsLand R 4, banana]

-- About List Monad
list1 :: [(Int, Int)]
list1 = [1, 2, 3] >>= \x -> [4, 5, 6] >>= \y -> return (x, y)

list2 :: [(Int, Int)]
list2 = do
  x <- [1, 2, 3]
  y <- [4, 5, 6]
  return (x, y)

list3 :: [(Int, Int)]
list3 = [(x, y) | x <- [1, 2, 3], y <- [4, 5, 6]]

type KnightLocation = (Int, Int)

type KnightPath = [KnightLocation]

(|>) :: a -> (a -> b) -> b
(|>) = flip ($)

makeAdvance :: KnightLocation -> KnightPath
makeAdvance loc = loc |> makePath |> filterOutside
  where
    makePath :: KnightLocation -> KnightPath
    makePath (x, y) =
      [ (x + 1, y + 2),
        (x + 1, y - 2),
        (x - 1, y + 2),
        (x - 1, y - 2),
        (x + 2, y + 1),
        (x + 2, y - 1),
        (x - 2, y + 1),
        (x - 2, y - 1)
      ]
    filterOutside :: KnightPath -> KnightPath
    filterOutside = filter isOnboard
      where
        isOnboard :: KnightLocation -> Bool
        isOnboard (x, y) = x `elem` [1 .. 8] && y `elem` [1 .. 8]

removeDuplicates :: (Eq a) => [a] -> [a]
removeDuplicates [] = []
removeDuplicates lst = check lst []
  where
    check :: (Eq a) => [a] -> [a] -> [a]
    check [] _ = []
    check (x : xs) checked = if x `elem` checked then check xs checked else x : (check xs (x : checked))

startLoc :: (Int, Int)
startLoc = (4, 5)

makeAdvanceNtimes :: KnightLocation -> Int -> KnightPath
makeAdvanceNtimes loc n = iterNtimes loc n |> removeDuplicates
  where
    iterNtimes :: KnightLocation -> Int -> KnightPath
    iterNtimes loc 0 = [loc]
    iterNtimes loc n = iterNtimes loc (n - 1) >>= makeAdvance
