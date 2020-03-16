{-# LANGUAGE TemplateHaskell, FlexibleContexts #-}
module Snake where

import           Control.Lens.TH                ( makeLenses )
import           Data.Sequence                  ( Seq
                                                , fromList
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
move = undefined

-- | Turn game direction (only turns orthogonally)
turn :: Direction -> Game -> Game
turn = undefined

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
