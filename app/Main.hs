module Main where

import Control.Monad
import System.IO
import Control.Concurrent (threadDelay)
import Options.Applicative


delay :: Int
delay = 100000

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  hSetBuffering stdin (BlockBuffering (Just 4096))
  let len = 30
  loop mempty len 0

  where
    loop :: [Char] -> Int -> Int -> IO ()

    loop acc l x = do
      eof <- isEOF
      -- print x
      if eof
        then do
          out acc
        else do
          chr <- getChar >>= \case
                  '\r' -> pure ' '
                  '\n' -> pure ' '
                  c    -> pure c

          let acc' = acc <> [chr]

          if x > l then do
            out acc'
            loop (drop 1 acc') l (succ x)
          else
            loop acc' l (succ x)



    out s = putStr s >> putChar '\r' >> threadDelay delay
