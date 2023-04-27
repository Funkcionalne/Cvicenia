module Result where

import Data.Char

data Result e v = Error [e] | Success v deriving Show

-- note that we are defining instance for (Result e) - why not for (Result e v), or even just Result
instance Functor (Result e) where
  fmap f (Error e) = Error e
  fmap f (Success s) = Success (f s)

instance Applicative (Result e) where
  pure v = Success v
  
  (Error a) <*> (Error b)= Error (a ++ b)
  (Error a) <*> _ = Error a
  _ <*> (Error b) = Error b
  (Success a) <*> (Success b) = Success (a b)

minLengthV :: Int -> String -> Result String String
minLengthV l v = if length v < l
                    then Error [v <> ": " <> "Minimum required length of input is " <> show l]
                    else Success v

maxLengthV :: Int -> String -> Result String String
maxLengthV l v = if length v > l
                    then Error [v <> ": " <> "Maximum allowed length of input is " <> show l]
                    else Success v

onlyAlphaNumericV :: String -> Result String String
onlyAlphaNumericV v = if all isAlpha v
                         then Success v
                         else Error [v <> ": " <> "Only alphanumeric characters allowed"]

numberV :: String -> Result String String
numberV v = if all isNumber v
                         then Success v
                         else Error [v <> ": " <> "Number expected"]
