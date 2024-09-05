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
safeHead1 (x:xs) = Just x

safeHead2 :: [a] -> Either String a
safeHead2 [] = Left "The list is empty."
safeHead2 (x:xs) = Right x

data List a = Nil | Cons a (List a) deriving (Eq, Show)

normalListToMyList :: [a] -> List a
normalListToMyList [] = Nil
normalListToMyList (x:xs) = Cons x (normalListToMyList xs)

data NaturalNumber = Zero | Succ NaturalNumber deriving (Eq, Show)

unSafeIntToNat :: Int -> NaturalNumber
unSafeIntToNat x
    | x == 0 = Zero
    | x > 0 = Succ $ unSafeIntToNat $ x - 1

intToNat :: Int -> Maybe NaturalNumber
intToNat x
    | x < 0 = Nothing
    | x == 0 = Just Zero
    | otherwise = (intToNat $ x - 1) >>= return . Succ

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

findElementFromTree :: (Ord a) => a -> Tree a -> Maybe String
findElementFromTree x Empty = Nothing
findElementFromTree x (Node a leftSub rightSub)
    | x == a = Just ""
    | x < a = (++) <$> Just "L" <*> findElementFromTree x leftSub
    | otherwise = (++) <$> Just "R" <*> findElementFromTree x rightSub

depthOfTree :: Tree a -> Int
depthOfTree Empty = 0
depthOfTree (Node _ leftSub rightSub) =
    if (depthOfTree leftSub) > (depthOfTree rightSub) 
        then 1 + depthOfTree leftSub
        else 1 + depthOfTree rightSub

type PropositionalLogic = Exp

type SymbolValues = [(String, Bool)]

data Exp = Not Exp | And (Exp, Exp) | Or (Exp, Exp) | Symbol String deriving (Eq, Show)

badEval :: SymbolValues -> PropositionalLogic -> Bool
badEval env (Symbol name) =
    let myLookup :: String -> SymbolValues -> Bool
        myLookup name (x:xs) = 
            if fst x == name
                then snd x
                else myLookup name xs in
    myLookup name env
badEval env (Not exp) = not (badEval env exp)
badEval env (And (e1, e2)) = (badEval env e1) && (badEval env e2)
badEval env (Or (e1, e2)) = (badEval env e1) && (badEval env e2)


eval :: SymbolValues -> PropositionalLogic -> Maybe Bool
eval env (Symbol name) = lookup name env
eval env (Not exp) = (eval env exp) >>= return.not
eval env (And (e1, e2)) = do
    x <- eval env e1
    y <- eval env e2
    return $ x && y
eval env (Or (e1, e2)) = do
    x <- eval env e1
    y <- eval env e2
    return $ x || y

symbols :: SymbolValues
symbols = [("x", True), ("y", False), ("z", True)]

exp1 :: PropositionalLogic
exp1 = And ((Or (Symbol "x", And(Not(Symbol "y"), Symbol "z"))), Symbol "y")
