{-# LANGUAGE NoMonomorphismRestriction #-}

module Main where

import Data.Json

x = obj
      [ ("foo", int 1)
      , ("bar", str "hi")
      , ("baz", arr
                [ int 1
                , num 2.0
                , str "3"
                , bool True
                , nul ])]

main :: IO ()
main = do
  let tree = toTree x
  print tree
  putStrLn $ toString x
  case fromTree tree of
    Left  e -> putStrLn $ "Error: " ++ e
    Right x' -> putStrLn $ toString x'
