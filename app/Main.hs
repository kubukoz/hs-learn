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

main :: IO ()
main = executeBoardProgram
--main = putStrLn "Hello World" *> prog1 *> prog2 *> prog3
