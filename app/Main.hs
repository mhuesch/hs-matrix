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
import           System.Console.Terminal.Size (Window(..), size)
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
  (height, width) <- do
    mW <- size
    case mW of
      Nothing -> error "wat"
      Just (Window h w) -> pure (h, w)
  let cs = replicate width (Column []) :: [Column]
  gen <- getStdGen
  _ <- go vty height gen cs
  shutdown vty

go :: Vty -> Int -> StdGen -> [Column] -> IO ()
go vty height gen cs = do
  threadDelay (5 * 10^(4::Int))
  let (cs', gen') = runRand (mapM (stepColumn height) cs) gen
      imgCols = map (renderColumn height) cs'
      pic = picForImage (horizCat imgCols)
  update vty pic
  mE <- nextEventNonblocking vty
  case mE of
    Just (EvKey KEsc _) -> pure ()
    Just (EvKey (KChar 'q') _) -> pure ()
    Just (EvKey (KChar 'c') [MCtrl]) -> pure ()
    _ -> go vty height gen' cs'

renderColumn :: Int -> Column -> Image
renderColumn height col = vertCat (V.toList vec)
 where
  vec :: Vector Image
  vec = runST $ do
    v <- MV.replicate height (char defAttr ' ')
    forM_ (col^.cSnakes) $ \snake -> do
      forM_ (zip [0..] (snake^.sBody)) $ \(offset, c) -> do
        let attr = if offset == 0
                      then (defAttr `withForeColor` white)
                      else (defAttr `withForeColor` green)
            idx = snake^.sHeadIdx - offset
        when (0 <= idx && idx < height) $
          MV.write v idx (char attr c)
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
spawnSnakes [] = (:[]) <$> spawnSnake
spawnSnakes (s:rest) | (snakeTailIdx s > 5) = (:s:rest) <$> spawnSnake
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
