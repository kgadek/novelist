module Main where


-- novelist
import           Lib (foo, bar)


main :: IO ()
main = do
    foo
    print . bar $ 123
