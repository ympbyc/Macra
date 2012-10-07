#! /usr/bin/env runhaskell
module Main where

import System.Environment(getArgs)
import Macra.Parser
import Macra.VM

main = do
  args <- getArgs
  case args of
    "--nodes":str:xs -> case parse "(fname)" str of
                             Left x -> print x
                             Right x -> print x