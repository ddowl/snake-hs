module Lib
    ( runSnake
    )
where

import           Brick
import           Brick.Widgets.Center
import           Brick.Widgets.Border
import           Brick.Widgets.Border.Style

snakePane :: Widget ()
snakePane = withBorderStyle unicode $ borderWithLabel
    (str "Snake!")
    (center (str "Snake is played here") <+> vBorder <+> fill '.')

statsPane :: Widget ()
statsPane = withBorderStyle unicode
    $ borderWithLabel (str "Stats") (center (str "Stats go here"))

ui :: Widget ()
ui = hLimitPercent 70 snakePane <+> statsPane

runSnake :: IO ()
runSnake = simpleMain ui
