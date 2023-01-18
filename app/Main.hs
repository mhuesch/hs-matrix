{-# LANGUAGE TemplateHaskell #-}
module Main where

import Graphics.Vty
import Lens.Micro.TH


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
  let line0 = string (defAttr ` withForeColor ` red) "first line"
      line1 = string (defAttr ` withBackColor ` blue) "second line"
      img = line0 <-> line1
      pic = picForImage img
  update vty pic
  e <- nextEvent vty
  shutdown vty
  print ("last event was: " ++ show e)
