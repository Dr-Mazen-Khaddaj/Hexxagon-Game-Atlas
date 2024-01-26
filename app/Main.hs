module Main where
import qualified MainFunction as MyLib

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  MyLib.someFunc
