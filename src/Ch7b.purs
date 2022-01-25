module Ch7b where

import Data.Eq (class Eq)
import Data.Generic.Rep (class Generic)
import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Show (class Show)
import Data.Show.Generic (genericShow)
import Data.String (Pattern(..), split)
import Effect (Effect)
import Effect.Console (log)
import Prelude (Unit, show, ($), (<>), (==), bind, pure, discard, (#))

test :: Effect Unit
test = do
  -- log $ show $ toCSV (Person {
  --     name: FullName "Dennis Dang"
  --   , age: Age 23
  --   , occupation: Doctor
  -- })
  log $ show $ toCSV (Person {
      name: FullName "Dennis Dang"
    , age: Age 23
    , occupation: Doctor
  }) == CSV "Dennis Dang,23,Doctor"
  let person = Person {
      name: FullName "Dennis Dang"
    , age: Age 23
    , occupation: Doctor
  }
  log $ show $ (toCSV person # fromCSV) == Just person


newtype CSV
  = CSV String
derive instance Newtype CSV _
derive newtype instance Eq CSV
derive newtype instance Show CSV

class ToCSV a where
  toCSV :: a -> CSV

instance ToCSV Person where
    toCSV (Person {name, age, occupation}) =
        CSV $ show name <> "," <> show age <> "," <> show occupation

newtype FullName = FullName String
instance Show FullName where
  show (FullName name) = name
derive instance Eq FullName

newtype Age = Age Int
derive instance Newtype Age _
derive newtype instance Show Age
derive instance Eq Age

data Occupation
  = Doctor
  | Dentist
  | Lawyer
  | Unemployed

derive instance Generic Occupation _
instance Show Occupation where
    show = genericShow
derive instance Eq Occupation


toOccupation :: String -> Maybe Occupation
toOccupation = case _ of 
  "Doctor" -> Just Doctor
  "Dentist" -> Just Dentist
  "Lawyer" -> Just Lawyer
  "Unemployed" -> Just Unemployed
  _ -> Nothing


data Person
  = Person
    { name :: FullName
    , age :: Age
    , occupation :: Occupation
    }

derive instance Eq Person

class FromCSV a where
  fromCSV :: CSV -> Maybe a

instance FromCSV Person where 
  fromCSV (CSV str) = case split (Pattern ",") str of
    [name, age, occupation] -> do 
      age' <- fromString age
      occupation' <- toOccupation occupation 
      pure $ Person { 
        name: FullName name
      , age: Age age'
      , occupation: occupation' }
    _ -> Nothing