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
  , _rand_pellets   :: [Int]
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
move g = g { _snake        = nextSnake
           , _pellet       = nextPellet
           , _rand_pellets = nextPellets
           , _score        = nextScore
           }
 where
  snake        = _snake g
  dir          = _dir g
  pellet       = _pellet g
  randPellets  = _rand_pellets g
  score        = _score g
  nextHead     = nextSnakeHead (snake `index` 0) dir
  withoutTail  = S.take (length snake - 1) snake
  eatingPellet = nextHead == pellet
  (nextPellets, nextPellet) =
    if eatingPellet then genPellet randPellets else (randPellets, pellet)
  nextSnake =
    if eatingPellet then nextHead <| snake else nextHead <| withoutTail
  nextScore = if eatingPellet then score + 10 else score

nextSnakeHead :: Coord -> Direction -> Coord
nextSnakeHead (x, y) North = (x, y + 1)
nextSnakeHead (x, y) South = (x, y - 1)
nextSnakeHead (x, y) East  = (x + 1, y)
nextSnakeHead (x, y) West  = (x - 1, y)

genPellet :: [Int] -> ([Int], Coord)
genPellet randPellets = (nextPellets, (x, y))
  where (x : y : nextPellets) = randPellets

-- | Turn game direction (only turns orthogonally)
turn :: Direction -> Game -> Game
turn d g = g { _dir = d }

-- | Initialize a paused game with random food location
initGame :: StdGen -> Game
initGame gen = Game { _snake        = fromList [(5, 5), (5, 4), (5, 3)]
                    , _dir          = North
                    , _pellet       = (3, 2)
                    , _stdGen       = gen
                    , _rand_pellets = randomRs (0, 14) gen
                    , _dead         = False
                    , _paused       = True
                    , _score        = 0
                    }
