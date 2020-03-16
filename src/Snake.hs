{-# LANGUAGE TemplateHaskell, FlexibleContexts #-}
module Snake where

import           Control.Lens.TH                ( makeLenses )
import           Data.Sequence                 as S
                                                ( Seq
                                                , (<|)
                                                , index
                                                , fromList
                                                , take
                                                )

type Coord = (Int, Int)
type Snake = Seq Coord

data Stream a = a :| Stream a
  deriving (Show)

data Direction
  = North
  | South
  | East
  | West
  deriving (Eq, Show)

data Game = Game
  { _snake          :: Snake
  , _dir            :: Direction
  , _pellet         :: Coord
  , _random_pellets :: Stream Coord
  , _dead           :: Bool
  , _paused         :: Bool
  , _score          :: Int
  } deriving (Show)
makeLenses ''Game

-- | Step forward in time
step :: Game -> Game
step = undefined

-- | Move snake along in a marquee fashion
move :: Game -> Game
move g = g { _snake = nextSnakeCoords (_snake g) (_dir g) }

nextSnakeCoords :: Snake -> Direction -> Snake
nextSnakeCoords s dir =
  nextSnakeHead (s `index` 0) dir <| S.take (length s - 1) s

nextSnakeHead :: Coord -> Direction -> Coord
nextSnakeHead (x, y) North = (x, y + 1)
nextSnakeHead (x, y) South = (x, y - 1)
nextSnakeHead (x, y) East  = (x + 1, y)
nextSnakeHead (x, y) West  = (x - 1, y)

-- | Turn game direction (only turns orthogonally)
turn :: Direction -> Game -> Game
turn d g = g { _dir = d }

-- | Initialize a paused game with random food location
initGame :: Game
initGame = Game { _snake          = fromList [(5, 3), (5, 4), (5, 5)]
                , _dir            = North
                , _pellet         = (3, 2)
                , _random_pellets = undefined
                , _dead           = False
                , _paused         = True
                , _score          = 0
                }
