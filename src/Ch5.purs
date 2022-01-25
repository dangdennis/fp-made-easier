module Ch5 where

import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)
import Prelude (Unit, (>), (+), (==), (>=), (/=), show, discard, negate, type (~>))

test :: Effect Unit
test = do
  log $ show $ flip const 1 2
  log $ show $ length $ 1 : 2 : 3 : Nil
  log $ show $ (head Nil :: Maybe Unit)
  log $ show $ head ("abc" : "123" : Nil)
  log $ show $ tail (Nil :: List Unit)
  log $ show $ tail ("abc" : "123" : Nil)
  log $ show $ (last Nil :: Maybe Unit)
  log $ show $ last ("a" : "b" : "c" : Nil)
  log $ show $ init (1 : Nil)
  log $ show $ init (1 : 2 : Nil)
  log $ show $ init (1 : 2 : 3 : Nil)
  log $ show $ index (1 : Nil) 4
  log $ show $ index (1 : 2 : 3 : Nil) 1
  log $ show $ index (Nil :: List Unit) 0
  log $ show $ (1 : 2 : 3 : Nil) !! 1
  log $ show $ findIndex (_ >= 2) (1 : 2 : 3 : Nil)
  log $ show $ findIndex (_ >= 99) (1 : 2 : 3 : Nil)
  log $ show $ findIndex (10 /= _) (Nil :: List Int)
  log $ show $ findLastIndex (_ == 10) (Nil :: List Int)
  log $ show $ findLastIndex (_ == 10) (10 : 5 : 10 : -1 : 2 : 10 : Nil)
  log $ show $ findLastIndex (_ == 10) (11 : 12 : Nil)
  log $ show $ reverse (10 : 20 : 30 : Nil)
  log $ show $ concat ((1 : 2 : 3 : Nil) : (4 : 5 : Nil) : (6 : Nil) : (Nil) : Nil)
  log $ show $ filter (4 > _) $ (1 : 2 : 3 : 4 : 5 : 6 : Nil)
  log $ show $ catMaybes (Just 1 : Nothing : Just 2 : Nothing : Nothing : Just 5 : Nil)

-- log $ show $ range 110
-- log $ show $ range 3 (-3)
const :: forall a b. a -> b -> a
const a _ = a

flip :: forall a b c. (a -> b -> c) -> b -> a -> c
flip f = \x y -> f y x

apply :: forall a b. (a -> b) -> a -> b
apply f a = f a

infixr 0 apply as $

applyFlipped :: ∀ a b. a -> (a -> b) -> b
applyFlipped = flip apply

infixl 0 applyFlipped as #

singleton :: forall a. a -> List a
singleton x = x : Nil

null :: forall a. List a -> Boolean
null Nil = true

null _ = false

snoc :: forall a. List a -> a -> List a
snoc Nil x = singleton x

snoc (y : ys) x = y : snoc ys x

length :: forall a. List a -> Int
length l = go 0 l
  where
  go :: Int -> List a -> Int
  go acc Nil = acc

  go acc (_ : xs) = go (acc + 1) xs

head :: forall a. List a -> Maybe a
head Nil = Nothing

head (x : _) = Just x

tail :: forall a. List a -> Maybe (List a)
tail Nil = Nothing

tail (_ : x) = Just x

last :: forall a. List a -> Maybe a
last Nil = Nothing

last (x : Nil) = Just x

last (_ : x) = last x

init :: forall a. List a -> Maybe (List a)
init Nil = Nothing

init l = Just $ go l
  where
  go Nil = Nil

  go (_ : Nil) = Nil

  go (x : xs) = x : go xs

uncons :: ∀ a. List a -> Maybe { head :: a, tail :: List a }
uncons Nil = Nothing

uncons (x : xs) = Just { head: x, tail: xs }

index :: forall a. List a -> Int -> Maybe a
index Nil _ = Nothing

index l i = go 0 l
  where
  go _ Nil = Nothing

  go ci (x : xs) = if ci == i then Just x else go (ci + 1) xs

infixl 8 index as !!

findIndex :: forall a. (a -> Boolean) -> List a -> Maybe Int
findIndex _ Nil = Nothing

findIndex pred l = go 0 l
  where
  go _ Nil = Nothing

  go i (x : xs) = if pred x then Just i else go (i + 1) xs

findLastIndex :: ∀ a. (a -> Boolean) -> List a -> Maybe Int
findLastIndex _ Nil = Nothing

findLastIndex pred l = go Nothing 0 l
  where
  go :: Maybe Int -> Int -> List a -> Maybe Int
  go fi _ Nil = fi

  go fi i (x : xs) = go (if pred x then Just i else fi) (i + 1) xs

reverse :: List ~> List
reverse Nil = Nil

reverse l = go Nil l
  where
  go hd Nil = hd

  go hd (x : xs) = go (x : hd) xs

concat :: forall a. List (List a) -> List a
concat Nil = Nil

concat (Nil : xss) = concat xss

concat ((x : xs) : xss) = x : concat (xs : xss)

filter :: forall a. (a -> Boolean) -> List a -> List a
filter pred l = reverse $ go Nil l
  where
  go nl Nil = nl

  go nl (x : xs) = if pred x then go (x : nl) xs else go nl xs

catMaybes :: forall a. List (Maybe a) -> List a
catMaybes Nil = Nil

catMaybes (x : xs) = case x of
  Just y -> y : catMaybes xs
  Nothing -> catMaybes xs

-- range :: Int -> Int -> List Int
-- range start end 
--   | start == end = singleton start
--   | otherwise = start : range (start + (if start < end then 1 else (-1))) end
