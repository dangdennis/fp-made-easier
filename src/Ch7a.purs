module Ch7a where

import Data.Eq (class Eq)
import Data.Ord (class Ord, Ordering(..), compare)
import Data.Show (class Show)
import Effect (Effect)
import Effect.Console (log)
import Prelude (Unit, show, discard, (==), ($), (<), (>), (<=), (||), (<>))
import Data.Generic.Rep (class Generic) 
import Data.Show.Generic (genericShow)

data Maybe a
  = Nothing
  | Just a

test :: Effect Unit
test = do
    log $ show $ Just 5 == Just 5
    log $ show $ Just 5 == Just 2
    log $ show $ Just 5 == Nothing
    log $ show $ Nothing == Just 5
    log $ show $ Nothing == (Nothing :: Maybe Unit)
    log "------------------"
    log $ show $ Just 1 < Just 5 -- COMPILER ERROR!!
    log $ show $ Just 5 <= Just 5
    log $ show $ Just 5 > Just 10
    log $ show $ Just 10 >= Just 10
    log $ show $ Just 99 > Nothing
    log $ show $ Just 99 < Nothing
    log "------------------"
    -- log $ show $ Just "abc"
    -- log $ show $ (Nothing :: Maybe Unit)

instance Eq a => Eq (Maybe a) where
    eq Nothing Nothing = true
    eq (Just x) (Just y) = x == y
    eq _ _ = false

instance Ord a => Ord (Maybe a) where
    compare Nothing Nothing = EQ
    compare (Just x) (Just y) =
        if x > y then GT else if x < y then LT else EQ
    compare Nothing _ = LT
    compare _ Nothing = GT

greaterThanOrEq :: forall a. Ord a => a -> a -> Boolean
greaterThanOrEq x y = cmp == GT || cmp == EQ where cmp = compare x y

infixl 4 greaterThanOrEq as >=

-- instance Show a => Show (Maybe a) where
--     show Nothing = "Nothing"
--     show (Just x) = "(Just " <> show x <> ")"