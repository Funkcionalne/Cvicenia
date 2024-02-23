module Result where

import Data.Char

data Result e v = Error [e] | Success v deriving Show

-- instance Functor (Result e) where
--   fmap f (Success x) = Success $ f x
--   fmap _ (Error e) = Error e
-- 
-- instance Applicative (Result e) where
--   pure = Success
--   Success f <*> Success x = Success (f x)
--   Error e <*> _ = Error e
--   _ <*> Error e = Error e
-- 
-- minLengthV :: Int -> String -> Result String String
-- minLengthV l v = if length v < l
--                     then Error (v <> ": " <> "Minimum required length of input is " <> show l)
--                    else Success v
-- 
-- maxLengthV :: Int -> String -> Result String String
-- maxLengthV l v = if length v > l
--                     then Error (v <> ": " <> "Maximum allowed length of input is " <> show l)
--                    else Success v
-- 
-- onlyAlphaNumericV :: String -> Result String String
-- onlyAlphaNumericV v = if all isAlpha v
--                          then Success v
--                          else Error (v <> ": " <> "Only alphanumeric characters allowed")
-- 
-- numberV :: String -> Result String String
-- numberV v = if all isNumber v
--                          then Success v
--                          else Error (v <> ": " <> "Number expected")

instance Functor (Result e) where
  fmap g (Success x) = Success $ g x
  fmap _ (Error x) = Error x

instance Applicative (Result e) where
  pure = Success
  Error   g <*> Success _ = Error g
  Success _ <*> Error   x = Error x
  Success g <*> Success x = Success (g x)
  Error   g <*> Error   x = Error (g <> x)

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
