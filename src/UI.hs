{-# LANGUAGE OverloadedStrings #-}
module UI
    ( runSnake
    )
where

import           Snake

import           Brick
import qualified Brick.Widgets.Center          as C
import qualified Brick.Widgets.Border          as B
import qualified Brick.Widgets.Border.Style    as BS
import qualified Graphics.Vty                  as V
import           Control.Lens                   ( (^.) )
import           Data.List                      ( intercalate )

type Name = ()
data Cell = Snake | Food | Empty

height = 15
width = 15

runSnake :: IO Game
runSnake = defaultMain app initGame

app :: App Game () Name
app = App { appDraw         = \g -> [ui g]
          , appChooseCursor = neverShowCursor
          , appHandleEvent  = handleEvent
          , appStartEvent   = return
          , appAttrMap      = const theMap
          }

-- Attribute map

theMap :: AttrMap
theMap = attrMap
    V.defAttr
    [ (snakeAttr   , V.blue `on` V.blue)
    , (foodAttr    , V.red `on` V.red)
    , (gameOverAttr, fg V.red `V.withStyle` V.bold)
    ]

-- Rendering

ui :: Game -> Widget Name
ui g = hLimitPercent 70 (snakePane g) <+> padLeft (Pad 2) (statsPane g)

snakePane :: Game -> Widget Name
snakePane g = withBorderStyle BS.unicodeRounded
    $ B.borderWithLabel (str "Snake!") grid
  where
    grid = vBox (map hBox rows)
    rows = [ cellsInRow r | r <- [height - 1, height - 2 .. 0] ]
    cellsInRow y = [ drawCoord (x, y) | x <- [0 .. width - 1] ]
    drawCoord = drawCell . cellAt
    cellAt c | c `elem` g ^. snake = Snake
             | c == g ^. pellet    = Food
             | otherwise           = Empty

drawCell :: Cell -> Widget Name
drawCell Snake = withAttr snakeAttr cw
drawCell Food  = withAttr foodAttr cw
drawCell Empty = withAttr emptyAttr cw

cw :: Widget Name
cw = str "  "

snakeAttr, foodAttr, emptyAttr, gameOverAttr :: AttrName
snakeAttr = "snakeAttr"
foodAttr = "foodAttr"
emptyAttr = "emptyAttr"
gameOverAttr = "gameOver"

-- TODO: Vertical/Horizontal limits should be related to the length of stats, with 1 row padding on each side
statsPane :: Game -> Widget Name
statsPane g = vLimit 6 $ withBorderStyle BS.unicodeRounded $ B.borderWithLabel
    (str "Stats")
    (C.center $ vBox $ map (str . mergeStatLabel) (stats g))

mergeStatLabel :: (String, Either Int Bool) -> String
mergeStatLabel (label, Left i ) = label ++ ": " ++ show i
mergeStatLabel (label, Right b) = label ++ ": " ++ show b

-- I don't like how I can't easily make a union on Int|Bool.
-- They both are typeclasses of Show, so why can't I return a type that impls Show?
stats :: Game -> [(String, Either Int Bool)]
stats g = [("Score", Left (g ^. score)), ("Alive", Right (not $ g ^. dead))]


-- Event handling
handleEvent :: Game -> BrickEvent Name () -> EventM Name (Next Game)
handleEvent g (VtyEvent (V.EvKey (V.KChar 'r') [])) = continue initGame
handleEvent g (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt g
handleEvent g (VtyEvent (V.EvKey V.KEsc [])) = halt g
handleEvent g _ = continue g
