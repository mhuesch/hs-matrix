{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Monad.Random (MonadRandom, runRand)
import Control.Monad.Random.Class (getRandomR)
import Graphics.Vty
import Lens.Micro ((^.), (%~), (&))
import Lens.Micro.TH
import System.Random (RandomGen)


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
  let initModel = [] :: [Column]
  _ <- go initModel
  shutdown vty
  putStrLn "bye 👋"

go :: [Column] -> IO ()
go model = do
  -- let line0 = string (defAttr ` withForeColor ` red) "first line"
  --     line1 = string (defAttr ` withBackColor ` blue) "second line"
  --     img = line0 <-> line1
  --     pic = picForImage img
  -- update vty pic
  -- e <- nextEvent vty
  pure ()

stepColumn :: MonadRandom m => Column -> m Column
stepColumn c = pure c

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

isSnakeOffscreen :: Int -> Snake -> Bool
isSnakeOffscreen height s = height < ((s ^. sHeadIdx) - (length (s ^. sBody)))
