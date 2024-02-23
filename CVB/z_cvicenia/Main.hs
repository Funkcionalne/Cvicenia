module Main where

import Control.Applicative (liftA2)
import Control.Monad (join)
import Control.Monad.State

import qualified Data.Map as M

-- recap
-- request
-- define using liftA2

(<<*>>) :: Applicative f => f (a -> b) -> f a -> f b
(<<*>>) = liftA2 id

-- define using <*>
myLiftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
myLiftA2 f x y = f <$> x <*> y

-- fmap and applicative - over IO?

-- use this with getLine - using fmap

greet :: String -> IO ()
greet = putStrLn . ("Hello " ++)

-- fmapom
autogreet :: IO ()
autogreet = join $ greet <$> getLine

-- (>>=)
autogreet' :: IO ()
autogreet' = getLine >>= greet

-- do notacia
autogreet'' :: IO ()
autogreet'' = undefined

-- what does the type IO (IO ()) say?

-- double all elements in a list
-- [1, 2, 3] -> [1, 1, 2, 2, 3, 3]

twice :: [a] -> [a]
twice xs = do 
         x <- xs
         [x, x]

-- maybeing

ageDB :: M.Map String Int
ageDB  = M.fromList [("mittens", 7), ("spot", 1), ("mia", 8)]

friendsDB :: M.Map String String
friendsDB = M.fromList [("mittens", "spot"), ("spot", "mittens"), ("mia", "nox")]

-- the most important computation in the world
-- computes the sum of a cats and its friends ages
friendsAgeSum :: String -> Maybe Int
friendsAgeSum name = do
    age <- M.lookup name ageDB
    friend <- M.lookup name friendsDB
    fAge <- M.lookup friend ageDB
    pure $ age + fAge
    
--
-- NOTE: we have pure in applicative which does the same as return in monads
-- see: https://gitlab.haskell.org/ghc/ghc/-/wikis/proposal/monad-of-no-return

-- type aliases are actually not that good
-- but here we will be able to talk better about
-- what we do during the exercise
type MonsterHP = Int
type Damage = Int

-- takes some amount of dmg from the monsters HP. does not go below zero
attack :: Damage -> State MonsterHP ()
attack dmg = do
    hp <- get
    put $ max 0 (hp - dmg)

-- checks whether the monster has more than 0 hp
isDead :: State MonsterHP Bool
isDead = do
    hp <- get
    return $ hp <= 0

-- try attacking a couple of times and see whether the monster is dead
fight :: State MonsterHP Bool
fight = do
    attack 10
    attack 10
    isDead

-- how to use multiple attacks defined in a list?
multipleAttacks :: [Damage] -> State MonsterHP ()
multipleAttacks = undefined

main :: IO ()
main = putStrLn "Hello, Haskell!"
