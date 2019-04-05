-- About typeclass

class Default a where
    getDefault :: a

instance Default Int where
    getDefault = 0

instance (Default a) => Default [a] where
    getDefault = []
    
instance (Default a) => Default (Maybe a) where
    getDefault = Just getDefault

instance (Default b) => Default (Either a b) where
    getDefault = Right getDefault


-- About functor
class MyFunctor f where
    myFmap :: (a -> b) -> f a -> f b

instance MyFunctor Maybe where
    myFmap _ Nothing = Nothing
    myFmap f (Just x) = Just $ f x

instance MyFunctor [] where
    myFmap _ [] = []
    myFmap f (x:xs) = (f x):(myFmap f xs)

instance MyFunctor (Either a) where
    myFmap _ (Left x) = Left x
    myFmap f (Right x) = Right $ f x

isItRightFunctor1 :: (Eq (f a), Functor f) => f a -> Bool
isItRightFunctor1 x = fmap id x == id x

isItRightFunctor2 :: (Eq (f c), Functor f) => (b -> c) -> (a -> b) -> f a -> Bool
isItRightFunctor2 f g x = fmap (f.g) x == fmap f (fmap g x)

-- Verify Maybe type
maybe1 = isItRightFunctor1 (Just 3)
maybe2 = isItRightFunctor2 (+3) (*5) (Just 4)

-- Type which doesn't obey functor laws

data BadMaybe a = BadJust Int a | BadNothing deriving (Eq, Show)

instance Functor BadMaybe where
    fmap f (BadJust n x) = BadJust (n + 1) (f x)
    fmap _ BadNothing = BadNothing

badMaybe1 = isItRightFunctor1 (BadJust 0 "asdf")
badMaybe2 = isItRightFunctor2 (+3) (+5) (BadJust 0 3)