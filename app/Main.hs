{-# LANGUAGE TemplateHaskell #-}
module Main where

import Graphics.Vty
import Lens.Micro ((^.), (%~), (&))
import Lens.Micro.TH
import System.Random (RandomGen, randomR)


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

stepColumn :: RandomGen g => g -> Column -> (Column, g)
stepColumn gen c = (c, gen)

stepSnake :: RandomGen g => g -> Snake -> (Snake, g)
stepSnake gen s = ( s & sHeadIdx %~ succ
                      & sBody %~ extendBody
                  , gen')
 where
  (c, gen') = genSnakeChar gen
  extendBody body = let len = length body
                     in take len (c:body)

-- | this exercises a decent chunk of the printable range
genSnakeChar :: RandomGen g => g -> (Char, g)
genSnakeChar = randomR ('!', '~')

isSnakeOffscreen :: Int -> Snake -> Bool
isSnakeOffscreen height s = height < ((s ^. sHeadIdx) - (length (s ^. sBody)))
