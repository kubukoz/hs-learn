{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Boards
  ( executeBoardProgram
  ) where

import           Control.Monad.IO.Class
import           Data.Foldable
import           Data.Maybe
import           Data.Semigroup
import           Prelude                hiding (Left, Right)
import           System.Environment
import           System.IO.Unsafe

newtype Coords =
  Coords (Sum Int, Sum Int)
  deriving (Semigroup)

newtype Rows =
  Rows [[Maybe Int]]

newtype Current =
  Current Int
  deriving (Show)

newtype Board =
  Board (Rows, Coords, Current)

instance Show Rows where
  show (Rows (rows)) = unlines . fmap (unwords . fmap (maybe " " show)) $ rows

instance Show Coords where
  show (Coords (Sum x, Sum y)) = "(" ++ show x ++ ", " ++ show y ++ ")"

instance Show Board where
  show (Board (rows, coord, current)) = show rows ++ "\n" ++ show coord ++ "\n" ++ show current ++ "\n"

newtype Move =
  Move Coords

allMoves :: [Move]
allMoves = fmap (Move . uncurry newCoords) [(-3, 0), (0, 3), (3, 0), (0, -3), (-2, 2), (2, 2), (2, -2), (-2, -2)]

toCoords :: Move -> Coords
toCoords (Move coords) = coords

newCoords :: Int -> Int -> Coords
newCoords x y = Coords (Sum x, Sum y)

-- I miss lenses...
modifyBoardCoords :: (Coords -> Coords) -> Board -> Board
modifyBoardCoords f (Board (table, coords, current)) = Board (table, f coords, current)

modifyBoardCurrent :: (Current -> Current) -> Board -> Board
modifyBoardCurrent f (Board (table, coords, current)) = Board (table, coords, f current)

getBoardCurrent :: Board -> Current
getBoardCurrent (Board (_, _, current)) = current

getBoardCoords :: Board -> Coords
getBoardCoords (Board (_, coords, _)) = coords

getBoardRows :: Board -> Rows
getBoardRows (Board (rows, _, _)) = rows

extractRows :: Rows -> [[Maybe Int]]
extractRows (Rows a) = a

extractCurrent :: Current -> Int
extractCurrent (Current c) = c

-- lenses over
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
  case modifyBoardCoords (moveCoords m) b of
    moved
      | inRange moved -> Just $ markNumber moved
    _ -> Nothing

emptyBoard :: Int -> Board
emptyBoard n = markNumber $ Board (table, newCoords 0 0, Current 0)
  where
    table = Rows $ tabs [1 .. n]
    tabs arr = Nothing <$ arr <$ arr

markNumberAt :: Coords -> Current -> Rows -> Rows
markNumberAt (Coords (Sum x, Sum y)) curr = Rows . marked . extractRows
  where
    marked = modifyAt y (modifyAt x (const . Just . extractCurrent $ curr))

modifyBoardRows :: (Rows -> Rows) -> Board -> Board
modifyBoardRows f (Board (rows, coords, current)) = Board (f rows, coords, current)

markNumber :: Board -> Board
markNumber board = modifyBoardRows (markNumberAt coords newCurrent) . modifyBoardCurrent (const newCurrent) $ board
  where
    coords = getBoardCoords board
    newCurrent = Current . (1 +) . extractCurrent . getBoardCurrent $ board

inRange :: Board -> Bool
inRange b = fitsX && fitsY && notTaken
  where
    Coords (Sum x, Sum y) = getBoardCoords b
    fitsX = fits x 0 (boardWidth b)
    fitsY = fits y 0 (boardHeight b)
    notTaken = isNothing $ (extractRows . getBoardRows $ b) !! y !! x

boardHeight :: Board -> Int
boardHeight = length . extractRows . getBoardRows

boardWidth :: Board -> Int
boardWidth = sum . fmap length . listToMaybe . extractRows . getBoardRows

isComplete :: Board -> Bool
isComplete = all (all isJust) . extractRows . getBoardRows

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
