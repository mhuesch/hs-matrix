{-# LANGUAGE TemplateHaskell #-}
module Main where

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
main = putStrLn "hi 👋"
