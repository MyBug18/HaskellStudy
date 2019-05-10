-- What is Monad?
class (Applicative m) => MyMonad m where
    (>>=.) :: m a -> (a -> m b) -> m b
    (>>.) :: m a -> m b -> m b
    a >>. b = a >>=. (\_ -> b)
    myReturn :: a -> m a
    {-# MINIMAL (>>=.) , myReturn #-}

instance MyMonad Maybe where
    (Just x) >>=. f = f x
    _ >>=. _ = Nothing
    myReturn x = Just x

instance MyMonad (Either a) where
    (Right x) >>=. f = f x
    (Left x) >>=. _ = Left x
    myReturn x = Right x

instance MyMonad [] where
    (x:xs) >>=. f = (f x) ++ (xs >>=. f)
    _ >>=. _ = []
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


type Clown = (Int, Int)
data Side = L | R deriving (Eq, Show)

begin :: Either String Clown
begin = return (0, 0)

isDangerous :: Clown -> Bool
isDangerous (l, r) = if abs (l - r) > 3 then True else False

birdsLand :: Side -> Int -> Clown -> Either String Clown
birdsLand side n (l, r) =
    let clown = if side == L then (l + n, r) else (l, r + n) in
        if isDangerous clown then Left "Balance breaked!" else Right clown

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