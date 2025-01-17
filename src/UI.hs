{-# LANGUAGE OverloadedStrings #-}
module UI
    ( runSnake
    )
where

import           Snake
import           Constants

import           Control.Monad                  ( forever )
import           Control.Concurrent             ( threadDelay
                                                , forkIO
                                                )
import           Brick
import           Brick.BChan                    ( newBChan
                                                , writeBChan
                                                )
import qualified Brick.Widgets.Center          as C
import qualified Brick.Widgets.Border          as B
import qualified Brick.Widgets.Border.Style    as BS
import qualified Graphics.Vty                  as V
import           Data.List                      ( intercalate )
import           System.Random                  ( getStdGen )

data Tick = Tick
type Name = ()
data Cell = Snake | Food | Empty

runSnake :: IO Game
runSnake = do
    gen  <- getStdGen
    chan <- newBChan 10
    forkIO $ forever $ do
        writeBChan chan Tick
        threadDelay delayNanos
    let buildVty = V.mkVty V.defaultConfig
    initialVty <- buildVty
    customMain initialVty buildVty (Just chan) app (initGame gen)

app :: App Game Tick Name
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
    rows = [ cellsInRow r | r <- [size - 1, size - 2 .. 0] ]
    cellsInRow y = [ drawCoord (x, y) | x <- [0 .. size - 1] ]
    drawCoord = drawCell . cellAt
    cellAt c | c `elem` snake g = Snake
             | c == pellet g    = Food
             | otherwise        = Empty

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
stats g = [("Score", Left (score g)), ("Alive", Right (not . dead $ g))]

-- Event handling
handleEvent :: Game -> BrickEvent Name Tick -> EventM Name (Next Game)
handleEvent g (VtyEvent (V.EvKey (V.KChar 'q') []       )) = halt g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'c') [V.MCtrl])) = halt g
handleEvent g (VtyEvent (V.EvKey V.KEsc        []       )) = halt g
handleEvent g _ | dead g                       = continue g
handleEvent g (AppEvent Tick                 ) = continue $ move g
handleEvent g (VtyEvent (V.EvKey V.KUp    [])) = continue $ turn North g
handleEvent g (VtyEvent (V.EvKey V.KDown  [])) = continue $ turn South g
handleEvent g (VtyEvent (V.EvKey V.KRight [])) = continue $ turn East g
handleEvent g (VtyEvent (V.EvKey V.KLeft  [])) = continue $ turn West g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'r') [])) =
    continue (initGame (stdGen g))
handleEvent g _ = continue g
