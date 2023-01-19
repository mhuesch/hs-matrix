{-# LANGUAGE TemplateHaskell #-}
module Main where

import           Control.Concurrent (threadDelay)
import           Control.Monad (forM_, replicateM, when)
import           Control.Monad.ST (runST)
import           Control.Monad.Random (MonadRandom, runRand)
import           Control.Monad.Random.Class (getRandomR)
import           Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import           Graphics.Vty
import           Lens.Micro ((^.), (%~), (&))
import           Lens.Micro.TH
import           System.Random (StdGen, getStdGen)


data Snake =
  Snake
    { _sBody :: [Char]
    , _sHeadIdx :: Int
    }
  deriving Show
makeLenses ''Snake

data Column =
  Column
    { _cSnakes :: [Snake]
    }
  deriving Show
makeLenses ''Column

main :: IO ()
main = do
  cfg <- standardIOConfig
  vty <- mkVty cfg
  gen <- getStdGen
  _ <- go vty gen []
  shutdown vty

go :: Vty -> StdGen -> [Column] -> IO ()
go vty gen cs = do
  threadDelay (5 * 10^(4::Int))
  (width, height) <-
    displayBounds (outputIface vty)
  -- in the event of width rescaling, pad / truncate cols
  let resizedCs = take width (cs <> repeat (Column []))
      (steppedCs, gen') = runRand (mapM (stepColumn height) resizedCs) gen
      imgCols = map (renderColumn height) steppedCs
      pic = picForImage (horizCat imgCols)
  update vty pic
  mE <- nextEventNonblocking vty
  case mE of
    Just (EvKey KEsc _) -> pure ()
    Just (EvKey (KChar 'q') _) -> pure ()
    Just (EvKey (KChar 'c') [MCtrl]) -> pure ()
    Just (EvKey (KChar ' ') _) -> pause vty gen' steppedCs
    _ -> go vty gen' steppedCs

pause :: Vty -> StdGen -> [Column] -> IO ()
pause vty gen cs = do
  e <- nextEvent vty
  case e of
    EvKey KEsc _ -> pure ()
    EvKey (KChar 'q') _ -> pure ()
    EvKey (KChar 'c') [MCtrl] -> pure ()
    EvKey (KChar ' ') _ -> go vty gen cs
    _ -> pause vty gen cs

renderColumn :: Int -> Column -> Image
renderColumn height col = vertCat (V.toList vec)
 where
  vec :: Vector Image
  vec = runST $ do
    v <- MV.replicate height (char defAttr ' ')
    forM_ (col^.cSnakes) $ \snake -> do
      forM_ (zip [0..] (snake^.sBody)) $ \(offset, c) -> do
        let color = if offset == 0 then white else green
            idx = snake^.sHeadIdx - offset
        when (0 <= idx && idx < height) $
          MV.write v idx (char (defAttr `withForeColor` color) c)
    V.unsafeFreeze v

stepColumn
  :: MonadRandom m
  => Int
  -> Column
  -> m Column
stepColumn height c = do
  snakesStepped <- mapM stepSnake (c ^. cSnakes)
  let snakesFiltered = filter (not . isSnakeOffscreen height) snakesStepped
  Column <$> spawnSnakes snakesFiltered

stepSnake :: MonadRandom m => Snake -> m Snake
stepSnake s = do
  c <- genSnakeChar
  pure (s & sHeadIdx %~ succ
          & sBody %~ extendBody c)
 where
  extendBody c body = let len = length body
                       in take len (c:body)

-- | this exercises a decent chunk of the printable range
genSnakeChar :: MonadRandom m => m Char
genSnakeChar = getRandomR ('!', '~')

spawnSnakes :: MonadRandom m => [Snake] -> m [Snake]
spawnSnakes [] = do
  p <- getRandomR (0,7::Int)
  if p < 1
     then ((:[]) <$> spawnSnake)
     else pure []
spawnSnakes (s:rest) | (snakeTailIdx s > 5) = do
  p <- getRandomR (0,7::Int)
  if p < 1
     then (:s:rest) <$> spawnSnake
     else pure (s:rest)
spawnSnakes (s:rest) = pure (s:rest)

spawnSnake :: MonadRandom m => m Snake
spawnSnake = do
  len <- getRandomR (5, 30)
  headIdx <- getRandomR (-30, -10)
  body <- replicateM len genSnakeChar
  pure (Snake body headIdx)

snakeTailIdx :: Snake -> Int
snakeTailIdx s = (s ^. sHeadIdx) - (length (s ^. sBody))

isSnakeOffscreen :: Int -> Snake -> Bool
isSnakeOffscreen height s = snakeTailIdx s >= height
