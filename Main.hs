module Main where

import RGM
import Control.Monad
import qualified Data.ByteString.Char8 as C

main = do content <- C.readFile "pricer.in"
          mapM_ (C.putStrLn . orderid) $ parseLogFile content
