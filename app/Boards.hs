{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TemplateHaskellQuotes      #-}

module Boards
  ( executeBoardProgram
  ) where

import           Control.Lens
import           Control.Monad.IO.Class
import           Data.Foldable
import           Data.Maybe
import           Data.Semigroup
import           Prelude                hiding (Left, Right)
import           System.Environment

newtype Coords =
  Coords (Sum Int, Sum Int)
  deriving (Semigroup)

newtype Rows =
  Rows [[Maybe Int]]

newtype Current =
  Current Int
  deriving (Show)

data Board = Board
  { _rows    :: Rows
  , _coords  :: Coords
  , _current :: Current
  }

makeBoard :: Rows -> Coords -> Current -> Board
makeBoard = Board

makeLenses ''Board

instance Show Rows where
  show (Rows (rows)) = unlines . fmap (unwords . fmap (maybe " " show)) $ rows

instance Show Coords where
  show (Coords (Sum x, Sum y)) = "(" ++ show x ++ ", " ++ show y ++ ")"

instance Show Board where
  show b = show (view rows b) ++ "\n" ++ show (view coords b) ++ "\n" ++ show (view current b) ++ "\n"

newtype Move =
  Move Coords

allMoves :: [Move]
allMoves = fmap (Move . uncurry newCoords) [(-3, 0), (0, 3), (3, 0), (0, -3), (-2, 2), (2, 2), (2, -2), (-2, -2)]

toCoords :: Move -> Coords
toCoords (Move coords) = coords

newCoords :: Int -> Int -> Coords
newCoords x y = Coords (Sum x, Sum y)

extractRows :: Rows -> [[Maybe Int]]
extractRows (Rows a) = a

extractCoords :: Coords -> (Int, Int)
extractCoords (Coords (Sum x, Sum y)) = (x, y)

extractCurrent :: Current -> Int
extractCurrent (Current c) = c

modifyAt :: Int -> (a -> a) -> [a] -> [a]
modifyAt _ _ []     = []
modifyAt 0 f (x:xs) = (f x) : xs
modifyAt n f (x:xs) = x : modifyAt (n - 1) f xs

fits :: Int -> Int -> Int -> Bool
fits what from until = what >= from && what < until

moveCoords :: Move -> Coords -> Coords
moveCoords = (<>) . toCoords

moveBoard :: Board -> Move -> Maybe Board
moveBoard b m =
  case over coords (moveCoords m) b of
    moved
      | inRange moved -> Just $ markNumber moved
    _ -> Nothing

emptyBoard :: Int -> Board
emptyBoard n = markNumber $ makeBoard table (newCoords 0 0) (Current 0)
  where
    table = Rows $ tabs [1 .. n]
    tabs arr = Nothing <$ arr <$ arr

markNumberAt :: Coords -> Current -> Rows -> Rows
markNumberAt coordies curr = Rows . marked . extractRows
  where
    (x, y) = extractCoords coordies
    marked = modifyAt y (modifyAt x (const . Just . extractCurrent $ curr))

modifyBoardRows :: (Rows -> Rows) -> Board -> Board
modifyBoardRows = over rows

markNumber :: Board -> Board
markNumber board = over rows (markNumberAt currentCoords newCurrent) . set current newCurrent $ board
  where
    currentCoords = view coords board
    newCurrent = Current . (1 +) . extractCurrent . view current $ board

inRange :: Board -> Bool
inRange b = fitsX && fitsY && notTaken
  where
    (x, y) = extractCoords . view coords $ b
    fitsX = fits x 0 (boardWidth b)
    fitsY = fits y 0 (boardHeight b)
    notTaken = isNothing $ (extractRows . view rows $ b) !! y !! x

boardHeight :: Board -> Int
boardHeight = length . extractRows . view rows

boardWidth :: Board -> Int
boardWidth = sum . fmap length . listToMaybe . extractRows . view rows

isComplete :: Board -> Bool
isComplete = all (all isJust) . extractRows . view rows

explore :: Board -> Maybe Board
explore board =
  case moved of
    [] -> Nothing
    remaining ->
      case find isComplete remaining of
        Nothing   -> listToMaybe $ mapMaybe explore remaining
        completed -> completed
  where
    moved = mapMaybe (moveBoard board) $ allMoves

showSolution :: Int -> IO ()
showSolution = maybe (pure ()) print . explore . emptyBoard

parseArg :: IO (Maybe Int)
parseArg = fmap listToMaybe getArgs >>= (traverse readIO)

executeBoardProgram :: IO ()
executeBoardProgram = fmap (fromMaybe 5) parseArg <* (putStrLn "Started work") >>= (liftIO . showSolution)
