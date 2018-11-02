{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import           Boards
import           Control.Monad
import           Control.Monad.IO.Class (MonadIO)
import           Data.Foldable
import           Data.Monoid
import           Lib

newtype Username = Username
  { extractU :: String
  } deriving (Show)

data User = User
  { username :: Username
  , age      :: Int
  }

tupleToUser (name, age) = User (Username name) age

data NonEmptyList a =
  NonEmptyList a
               [a]
  deriving (Show, Functor, Foldable)

tuples = NonEmptyList ("kubukoz", 21) [("kumalg", 22), ("starbuxman", 34), ("g00bm4n", 22)]

friends = fmap tupleToUser tuples

prog1 = print . fmap extractU . fmap username $ friends

prog2 = print . getSum . foldMap (Sum . age) $ friends

div3 a = rem a 3 == 0

prog3 = do
  count <- getLine >>= readIO
  let numbers = filter div3 . take count $ [1 ..]
  traverse_ print numbers
  when (null numbers) retry
  where
    retry = (putStrLn "Didn't find any, try again" *> prog3)

main :: IO ()
main = executeBoardProgram
--main = putStrLn "Hello World" *> prog1 *> prog2 *> prog3
