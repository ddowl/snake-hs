module Lib
    ( runSnake
    )
where

import           Brick
import           Brick.Widgets.Center
import           Brick.Widgets.Border
import           Brick.Widgets.Border.Style

type Coord = (Int, Int)
data GameState = GameState { pellet :: Coord, snake :: [Coord] }

currState = GameState { pellet = (0, 0), snake = [(5, 3), (5, 4), (5, 5)] }

snakePane :: Widget ()
snakePane = withBorderStyle unicodeRounded
    $ borderWithLabel (str "Snake!") (center (str "Snake is played here"))

statsPane :: Widget ()
statsPane = withBorderStyle unicodeRounded
    $ borderWithLabel (str "Stats") (center (str "Stats go here"))

ui :: Widget ()
ui = hLimitPercent 70 snakePane <+> statsPane

runSnake :: IO ()
runSnake = simpleMain ui
