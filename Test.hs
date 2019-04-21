module Test where

import Grammar

import Control.Category ((>>>))
import Data.Aeson       (Value(..))
import Data.Text        (Text)

import qualified Data.HashMap.Strict as HashMap

data Person
  = Person
  { name :: Text
  , age :: Int
  } deriving (Show)

personGrammar :: Grammar (Value, x) (Person, x)
personGrammar =
  strictObject
    (key "name" string >>>
      key "age" integral)
  >>> syntax to from

  where
    to (age, (name, x)) = Just (Person name age, x)
    from (person, x) = Just ((age person, (name person, x)))

person :: Person
person =
  Person
    { name = "Bob"
    , age = 15
    }

personValue :: Value
personValue =
  Object
    (HashMap.fromList
      [ ("name", String "Bob")
      , ("age", Number 15)
      , ("extra", Bool True)
      ])
