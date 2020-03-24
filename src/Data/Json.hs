{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Data.Json
  ( Json(..)
  , toString
  , Tree
  , toTree
  , fromTree
  ) where

import Control.Monad


class Json repr where
  str :: String -> repr
  num :: Double -> repr
  int :: Int -> repr
  bool :: Bool -> repr
  nul :: repr
  arr :: [repr] -> repr
  obj :: [(String, repr)] -> repr


instance Json String where
  str = show
  num = show
  int = show
  bool b
    | True = "true"
    | False = "false"
  nul = "null"
  arr xs = "[" ++ (foldl element "" xs) ++ "]"
    where
      element "" x = x
      element acc x = acc ++ "," ++ x
  obj xs = "{" ++ (foldl member "" xs) ++ "}"
    where
      member "" x = mem x
      member acc x = acc ++ "," ++ mem x
      mem (k,v) = (show k) ++ ":" ++ v


toString :: String -> String
toString = id


data Tree = Leaf String | Node String [Tree]
  deriving (Eq, Read, Show)

instance Json Tree where
  str s = Node "Str" [Leaf s]
  num = nodeLeaf "Num"
  int = nodeLeaf "Int"
  bool = nodeLeaf "Bool"
  nul = Node "Null" []
  arr = Node "Arr"
  obj xs = Node "Obj" $ map member xs
    where
      member (k, v) = Node "Member" [Leaf k, v]

nodeLeaf :: Show a => String -> a -> Tree
nodeLeaf t x = Node t [Leaf $ show x]

toTree :: Tree -> Tree
toTree = id


type ErrMsg = String

safeRead :: Read a => String -> Either ErrMsg a
safeRead s = case reads s of
        [(x,"")] -> Right x
        _        -> Left $ "Read error: " ++ s

fromTree :: (Json repr) => Tree -> Either ErrMsg repr
fromTree (Node "Str" [Leaf s]) = Right $ str s
fromTree (Node "Num" [Leaf x]) = liftM num $ safeRead x
fromTree (Node "Int" [Leaf x]) = liftM int $ safeRead x
fromTree (Node "Bool" [Leaf x]) = liftM bool $ safeRead x
fromTree (Node "Null" []) = Right nul
fromTree (Node "Arr" xs) = liftM arr (mapM fromTree xs)
fromTree (Node "Obj" xs) = liftM obj (mapM member xs)
  where member (Node "Member" [Leaf k, v]) = either (k, fromTree v)
        either (k, Right x) = Right (k, x)
        either (k, Left e) = Left e
fromTree e = Left $ "Invalid tree: " ++ show e
