{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Concurrent (threadDelay)
import Control.Monad (replicateM)
import Control.Monad.Random (MonadRandom, runRand)
import Control.Monad.Random.Class (getRandomR)
import Graphics.Vty
import Lens.Micro ((^.), (%~), (&))
import Lens.Micro.TH
import System.Random (RandomGen, StdGen, getStdGen)


data Snake =
  Snake
    { _sBody :: [Char]
    , _sHeadIdx :: Int
    }
makeLenses ''Snake

data Column =
  Column
    { _cSnakes :: [Snake]
    }
makeLenses ''Column

main :: IO ()
main = do
  cfg <- standardIOConfig
  vty <- mkVty cfg
  let cs = replicate width [] :: [Column]
  gen <- getStdGen
  _ <- go vty gen cs
  shutdown vty
  putStrLn "bye 👋"

go :: Vty -> StdGen -> [Column] -> IO ()
go vty gen cs = do
  threadDelay (10^5)
  let (cs', gen') = runRand (mapM (stepColumn height) cs) gen
  let line0 = string (defAttr `withForeColor` red) (show cs')
      pic = picForImage line0
  update vty pic
  go vty gen' cs'

stepColumn
  :: MonadRandom m
  => Int
  -> Column
  -> m Column
stepColumn height c = do
  snakesStepped <- mapM stepSnake (c ^. cSnakes)
  let snakesFiltered = filter (isSnakeOffscreen height) snakesStepped
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
spawnSnakes (s:rest) | (s^.sHeadIdx > 5) = (:s:rest) <$> spawnSnake
spawnSnakes (s:rest) = pure (s:rest)

spawnSnake :: MonadRandom m => m Snake
spawnSnake = do
  len <- getRandomR (5, 30)
  headIdx <- getRandomR (-30, -10)
  body <- replicateM len genSnakeChar
  pure (Snake body headIdx)

isSnakeOffscreen :: Int -> Snake -> Bool
isSnakeOffscreen height s = height < ((s ^. sHeadIdx) - (length (s ^. sBody)))
