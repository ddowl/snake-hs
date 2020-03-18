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
import           System.Random                  ( Random(..)
                                                , StdGen
                                                , getStdGen
                                                , next
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
  , _stdGen         :: StdGen
  , _dead           :: Bool
  , _paused         :: Bool
  , _score          :: Int
  }
makeLenses ''Game

-- | Step forward in time
step :: Game -> Game
step = undefined

-- | Move snake along in a marquee fashion
move :: Game -> Game
move g = g { _snake = nextSnake, _pellet = nextPellet, _stdGen = nextStdGen }
 where
  snake        = _snake g
  dir          = _dir g
  pellet       = _pellet g
  stdGen       = _stdGen g
  nextHead     = nextSnakeHead (snake `index` 0) dir
  withoutTail  = S.take (length snake - 1) snake
  eatingPellet = nextHead == pellet
  (nextStdGen, nextPellet) =
    if eatingPellet then genPellet stdGen pellet else (stdGen, pellet)
  nextSnake =
    if eatingPellet then nextHead <| snake else nextHead <| withoutTail

nextSnakeHead :: Coord -> Direction -> Coord
nextSnakeHead (x, y) North = (x, y + 1)
nextSnakeHead (x, y) South = (x, y - 1)
nextSnakeHead (x, y) East  = (x + 1, y)
nextSnakeHead (x, y) West  = (x - 1, y)

genPellet :: StdGen -> Coord -> (StdGen, Coord)
genPellet stdGen pellet = (nextGen, (x, y))
 where
  (x, g      ) = randomR (0, 14) stdGen
  (y, nextGen) = randomR (0, 14) g

-- | Turn game direction (only turns orthogonally)
turn :: Direction -> Game -> Game
turn d g = g { _dir = d }

-- | Initialize a paused game with random food location
initGame :: StdGen -> Game
initGame gen = Game { _snake  = fromList [(5, 5), (5, 4), (5, 3)]
                    , _dir    = North
                    , _pellet = (3, 2)
                    , _stdGen = gen
                    , _dead   = False
                    , _paused = True
                    , _score  = 0
                    }
