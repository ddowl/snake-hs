{-# LANGUAGE FlexibleContexts #-}
module Snake where

import           Data.Sequence                 as S
                                                ( Seq(..)
                                                , (<|)
                                                , index
                                                , fromList
                                                , take
                                                )
import           System.Random                  ( Random(..)
                                                , StdGen
                                                , getStdGen
                                                , next
                                                )

type Coord = (Int, Int)
type Snake = Seq Coord

data Direction
  = North
  | South
  | East
  | West
  deriving (Eq, Show)

data Game = Game
  { snake          :: Snake
  , dir            :: Direction
  , pellet         :: Coord
  , stdGen         :: StdGen
  , randPellets    :: [Int]
  , dead           :: Bool
  , paused         :: Bool
  , score          :: Int
  }

-- | Move snake along in a marquee fashion
move :: Game -> Game
move g = g { snake       = nextSnake
           , pellet      = nextPellet
           , randPellets = nextPellets
           , score       = nextScore
           , dead        = isGameOver
           }
 where
  isGameOver                = isOutOfBounds (snake g) || isOverlapping (snake g)
  snakeHead :<| _           = snake g
  withoutTail               = S.take (length (snake g) - 1) (snake g)
  nextHead                  = nextSnakeHead snakeHead (dir g)
  eatingPellet              = nextHead == pellet g
  (x : y : mabeNextPellets) = randPellets g
  (nextSnake, nextScore, nextPellets, nextPellet) = if eatingPellet
    then (nextHead <| snake g, score g + 10, mabeNextPellets, (x, y))
    else (nextHead <| withoutTail, score g, randPellets g, pellet g)

nextSnakeHead :: Coord -> Direction -> Coord
nextSnakeHead (x, y) North = (x, y + 1)
nextSnakeHead (x, y) South = (x, y - 1)
nextSnakeHead (x, y) East  = (x + 1, y)
nextSnakeHead (x, y) West  = (x - 1, y)

isOutOfBounds :: Snake -> Bool
isOutOfBounds ((x, y) :<| _) = x < 0 || x >= 15 || y < 0 || y >= 15

isOverlapping :: Snake -> Bool
isOverlapping (h :<| rest) = h `elem` rest

-- | Turn game direction (only turns orthogonally)
turn :: Direction -> Game -> Game
turn North g | dir g == South = g
turn South g | dir g == North = g
turn East g | dir g == West   = g
turn West g | dir g == East   = g
turn d g                      = g { dir = d }

-- | Initialize a paused game with random food location
initGame :: StdGen -> Game
initGame gen = Game { snake       = fromList [(5, 5), (5, 4), (5, 3)]
                    , dir         = North
                    , pellet      = (3, 2)
                    , stdGen      = gen
                    , randPellets = randomRs (0, 14) gen
                    , dead        = False
                    , paused      = True
                    , score       = 0
                    }
