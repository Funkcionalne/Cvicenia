module Main where

import Data.Char
import qualified Data.Map as M

import Result

-- WARMUP

-- understanding

-- fmap over various datatypes
-- apply functions of arbitrary number of arguments
-- * add 2 Maybe Int values
-- * multiply 2 Either String Int values
-- * add 2 [Int] values
-- ** what do you see here?
-- * define a function of three arguments and use it with a wrapped type

-- function rewriting

data Address = Address { street :: String
                       , number :: Int
                       , city   :: String
                       } deriving Show

type Database = M.Map String Address

db :: Database
db = M.fromList [("joe", Address "Obchodna" 22 "Bratislava")]

getCity :: Database -> String -> Maybe String
getCity db name = case M.lookup name db of
  Just address -> Just $ city address
  Nothing      -> Nothing

nicerGetCity :: Database -> String -> Maybe String
nicerGetCity db name = let address = M.lookup name db
                        in city <$> address

-- instance writing
data Tsil a = Tsil a :< a | Empty deriving (Show)

instance Functor Tsil where
  fmap _ Empty = Empty
  fmap f (xs :< x) = fmap f xs :< f x

instance Applicative Tsil where
  pure x = Empty :< x
  Empty <*> _ = Empty
  _ <*> Empty = Empty
  (fs :< f) <*> (xs :< x) = (fs <*> xs) :< f x

-- test the implementation

-- overriding an instance - List

-- this is Data.Functor.ZipList
newtype SimplerList a = SimplerList { asList :: [a] } deriving (Show)

instance Functor SimplerList where
  fmap f (SimplerList xs) = SimplerList (fmap f xs)

instance Applicative SimplerList where
  pure x = SimplerList [x]
  SimplerList [] <*> _ = SimplerList []
  _ <*> SimplerList [] = SimplerList []
  SimplerList fs <*> SimplerList xs = SimplerList $ (\(f, x) -> f x) <$> zip fs xs
  -- or rather
  -- SimplerList fs <*> SimplerList xs = SimplerList $ zipWith ($) fs xs

-- more serious example

mkAddress :: String -> String -> String -> Address
mkAddress s n c = Address { street = s
                          , number = read n
                          , city   = c }

getAddress :: IO ()
getAddress = do
  putStrLn "Enter your address in format \"street number city\": "
  userInputs <- words <$> getLine -- what happens on this line?

  if length userInputs /= 3
  then do
     putStrLn "Wrong format :("
     getAddress
  else do
     let [streetInput, numberInput, cityInput] = userInputs
     let address = mkAddress <$> minLengthV 5 streetInput
                             <*> numberV numberInput
                             <*> minLengthV 5 cityInput
     print address

-- multiple inputs validated

getNickName :: IO ()
getNickName = do
  putStrLn "Enter your nickname: "
  input <- getLine

  let validated =
        minLengthV 5 input
        *> maxLengthV 20 input
        *> onlyAlphaNumericV input

  print $ ("Hello " ++) <$> validated


main :: IO ()
main = getAddress
