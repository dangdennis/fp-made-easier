module Ch13
  where

-- import Data.Bifunctor (bimap, lmap, rmap)
import Data.Eq (class Eq)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.String.Common (toUpper)
import Effect (Effect)
import Effect.Console (log)
import Prelude ((<<<), identity, Unit, show, ($), (*), (&&), discard, (==), (/), class Show, (<>))


test :: Effect Unit
test = do 
    log $ show $ (_ / 2) <$> Just 10
    log $ show $ (_ / 2) <$> Nothing
    log $ show $ (_ / 2) <$> (Right 10 :: Either Unit _)
    log $ show $ (_ / 2) <$> Left "error reason"
    log $ show $ (_ / 2) <$> Tuple 10 20
    log $ show $ (_ / 2) <$> Threeple 10 20 40
    log $ show $ "Maybe Identity for Nothing: " <> show ((identity <$> Nothing) == (Nothing :: Maybe Unit))
    log $ show $ "Maybe Identity for Just: " <> show ((identity <$> Just [1, 2]) == Just [1, 2])
    let g x = x * 2
        f x = x * 3
    log $ show $ "Maybe Composition for Nothing: " <> show (map (g <<< f) Nothing == (map g <<< map f) Nothing)
    log $ show $ "Maybe Composition for Just: " <> show (map (g <<< f) (Just 5) == (map g <<< map f) (Just 5))
    -- log $ show $ rmap (_ * 2) $ Left "error reason"
    -- log $ show $ rmap (_ * 2) $ (Right 10 :: Either Unit _)
    -- log $ show $ lmap toUpper $ (Left "error reason" :: Either _ Unit)
    -- log $ show $ lmap toUpper $ Right 10
    
class Functor f where
    map :: ∀ a b . (a -> b) -> f a -> f b
infixl 4 map as <$>

data Maybe a = Nothing | Just a
derive instance Eq a => Eq (Maybe a)

instance Functor Maybe where
    map _ Nothing = Nothing
    map f (Just x) = Just (f x)

derive instance Generic (Maybe a) _
instance Show a => Show (Maybe a) where
    show = genericShow

data Either a b = Left a | Right b
derive instance Generic (Either a b) _
instance (Show a, Show b) => Show (Either a b) where
    show = genericShow

instance Functor (Either a) where
    map _ (Left err) = Left err
    map f (Right x) = Right $ f x

data Tuple a b = Tuple a b
derive instance Generic (Tuple a b) _
instance (Show a, Show b) => Show (Tuple a b) where
    show = genericShow

instance Functor (Tuple a) where
    map f (Tuple x y) = Tuple x $ f y

data Threeple a b c = Threeple a b c
derive instance Generic (Threeple a b c) _
instance (Show a, Show b, Show c) => Show (Threeple a b c) where
    show = genericShow

instance Functor (Threeple a b) where
    map f (Threeple a b c) = Threeple a b $ f c


-- Skipped the bifunctor section.