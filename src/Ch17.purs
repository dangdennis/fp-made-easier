module Ch17
  where

import Prelude (class Semigroup, class Eq, class Ord, discard, class Show,negate, Unit, show, (<>), ($), (==), (<<<),identity, (<$>), (+), (*), (<*>), pure, class Functor, class Apply, class Applicative)

import Data.Bifunctor (class Bifunctor)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.Console (log)

test :: Effect Unit
test = do
    log $ show $ (+) <$> Just 21 <*> Just 21
    log $ show $ (*) <$> pure 2 <*> (pure 21 :: Maybe Int)
    log $ show $ pure (+) <*> Just 17 <*> Just 25
    log $ show $ ((<<<) <$> pure identity <*> pure identity <*> pure 1) == (pure identity <*> (pure identity <*> pure 1) :: Either Unit Int)
    log $ show $ (pure identity <*> pure 1) == (pure 1 :: Either Unit Int)
    log $ show $ pure (negate 1) == (pure negate <*> pure 1 :: Either Unit Int)
    log $ show $ (pure negate <*> pure 1) == (pure (_ $ 1) <*> pure negate :: Either Unit Int)

data Maybe a = Nothing | Just a

derive instance Generic (Maybe a) _
instance Show a => Show (Maybe a) where
    show = genericShow

instance Functor Maybe where
    map _ Nothing = Nothing
    map f (Just a) = Just (f a)

instance Apply Maybe where
    apply (Just f) x = f <$> x
    apply Nothing _ = Nothing

instance Applicative Maybe where
    pure = Just

data Either a b = Left a | Right b
derive instance (Eq a, Eq b) => Eq (Either a b)
derive instance (Ord a, Ord b) => Ord (Either a b)
derive instance Functor (Either a)
derive instance Generic (Either a b) _
instance (Show a, Show b) => Show (Either a b) where
    show = genericShow
instance Bifunctor Either where
    bimap f _ (Left x) = Left (f x)
    bimap _ g (Right y) = Right (g y)
instance Apply (Either a) where
    apply (Right f) x = f <$> x
    apply (Left y) _ = Left y
instance Applicative (Either a) where
    pure = Right

newtype Validation err result = Validation (Either err result)
derive instance Newtype (Validation err result) _
derive newtype instance Functor (Validation err)
derive newtype instance Bifunctor (Validation)
derive instance (Eq err, Eq result) => Eq (Validation err result)
derive instance (Ord err, Ord result) => Ord (Validation err result)
instance Semigroup err => Apply (Validation err) where
    apply (Validation (Left err1)) (Validation (Left err2)) 
        = Validation $ (Left (err1 <> err2))
    apply (Validation (Left err)) _ = Validation (Left err)
    apply (Validation (Right f)) x = f <$> x
instance Semigroup err => Applicative (Validation err) where
    pure x = Validation (Right x)
derive instance Generic (Validation err result) _
instance (Show err, Show result) => Show (Validation err result) where
    show = genericShow

newtype Age = Age Int
derive instance Generic Age _
instance Show Age where
    show = genericShow
newtype FullName = FullName String
derive instance Generic FullName _
instance Show FullName where
    show = genericShow
type FamilyAgesRow r = (fatherAge :: Age, motherAge :: Age, childAge :: Age | r)
type FamilyNamesRow r = (fatherName :: FullName, motherName :: FullName, childName :: FullName | r)
newtype Family = Family { | FamilyNamesRow (FamilyAgesRow ())}
derive instance genericFamily :: Generic Family _
instance showFamily :: Show Family where 
    show = genericShow