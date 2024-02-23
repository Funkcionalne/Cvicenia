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

-- pattern matching on the result? meh
getCity :: Database -> String -> Maybe String
getCity db name = case M.lookup name db of
  Just address -> Just $ city address
  Nothing      -> Nothing

-- we can do better with what we know now
nicerGetCity :: Database -> String -> Maybe String
nicerGetCity db name =  city <$> M.lookup name db

-- instance writing
-- data Tsil a = Tsil a :< a | Empty deriving (Show)

data MyList a = Cons a (MyList a) | Empty deriving Show


-- Functor instance
instance Functor MyList where
    fmap f Empty = Empty
    fmap f (Cons a as) = Cons (f a) (fmap f as)

-- Applicative instance
instance Applicative MyList where
    (<*>) _ Empty = Empty
    Empty <*> _ = Empty
    (Cons a as) <*> (Cons b bs) = Cons (a b) (as <*> bs)

    pure a = Cons a Empty
-- test the implementation

-- overriding an instance - List
-- can you think of an instance that would break the laws?

-- this is Data.Functor.ZipList
newtype SimplerList a = SimplerList { asList :: [a] } deriving (Show)

-- Functor instance
-- Applicative instance

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
