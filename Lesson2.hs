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

just6 :: Maybe Int
just6 = fmap (+3) $ Just 3

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

justList :: [Maybe Int]
justList = (fmap.fmap) (+3) [Just 3, Just 5, Nothing]

-- About applicative

just7 :: Maybe Int
just7 = Just (+3) <*> Just 5

class MyApplicative f where
    myPure :: a -> f a
    (<*>.) :: f (a -> b) -> f a -> f b

instance MyApplicative Maybe where
    myPure x = Just x
    (Just f) <*>. (Just x) = Just $ f x
    _ <*>. _ = Nothing

instance MyApplicative [] where
    myPure x = x:[]
    (f:fs) <*>. xs = (fmap f xs) ++ (fs <*>. xs)
    _ <*>. _ = []

instance MyApplicative (Either a) where
    myPure x = Right x
    (Right f) <*>. (Right x) = Right $ f x
    (Left e) <*>. _ = Left e
    _ <*>. (Left e) = Left e

type Age = Int
type Name = String
data Gender = Male | Female | Neutrallized deriving (Eq, Show)

data Dog = MyDog Age Name Gender deriving (Eq, Show)

makeValidDog1 :: Age -> Name -> Gender -> Maybe Dog
makeValidDog1 age name gender =
    if age > 20
        then Nothing
        else if length name < 4
            then Nothing 
            else if gender == Neutrallized
                then Nothing
                else Just (MyDog age name gender)


validateAge :: Age -> Maybe Age
validateAge age = if age > 20 then Nothing else Just age
validateName :: Name -> Maybe Name
validateName name = if length name < 3 then Nothing else Just name
validateGender :: Gender -> Maybe Gender
validateGender gender = if gender == Neutrallized then Nothing else Just gender

makeValidDog2 :: Age -> Name -> Gender -> Maybe Dog
makeValidDog2 age name gender = pure MyDog <*> validateAge age <*> validateName name <*> validateGender gender

data MyEither a b = MyLeft [a] | MyRight b deriving (Eq, Show)

instance Functor (MyEither a) where
    fmap _ (MyLeft xs) = MyLeft xs
    fmap f (MyRight b) = MyRight $ f b

instance Applicative (MyEither a) where
    pure x = MyRight x
    MyRight f <*> MyRight x = MyRight $ f x
    MyLeft xs <*> MyLeft ys = MyLeft $ xs ++ ys
    MyLeft xs <*> _ = MyLeft xs
    _ <*> MyLeft xs = MyLeft xs


type IsWoman = Bool
type Weight = Float
type EyeSight = Float
type IsInsane = Bool

data Soldier = Slave IsWoman IsInsane Weight EyeSight deriving (Eq, Show)
data Exempt = Woman | Insane | Obesity | EyeSightLow deriving (Eq, Show)

isWoman :: IsWoman -> MyEither Exempt IsWoman
isWoman woman = if woman then MyLeft [Woman] else MyRight False

isInsane :: IsInsane -> MyEither Exempt IsInsane
isInsane insane = if insane then MyLeft [Insane] else MyRight False

isFat :: Weight -> MyEither Exempt Weight
isFat weight = if weight > 100.0 then MyLeft [Obesity] else MyRight weight

isBlind :: EyeSight -> MyEither Exempt EyeSight
isBlind eye = if eye < 0.0 then MyLeft [EyeSightLow] else MyRight eye

recruitSoldier :: IsWoman -> IsInsane -> Weight -> EyeSight -> MyEither Exempt Soldier
recruitSoldier woman insane weight eye =
    pure Slave <*> isWoman woman <*> isInsane insane <*> isFat weight <*> isBlind eye
    