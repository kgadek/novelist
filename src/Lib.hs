module Lib (
  foo
, bar
) where


foo :: IO ()
foo = putStrLn "foo"

bar :: Int -> Int
bar = (1+)
