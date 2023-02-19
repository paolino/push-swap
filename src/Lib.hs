{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Lib (main) where

import Control.Lens (makeLenses, over)
import Data.Set qualified as Set
import Data.String (String)
import Protolude hiding (rotate, swap)
import Protolude.Unsafe (unsafeInit, unsafeLast, unsafeTail)
import Text.Printf (printf)

type Stack a = [a]

-- | A Board is just a pair of stacks
data Board a = Board {_front :: Stack a, _back :: Stack a}
  deriving (Show, Eq, Ord)

makeLenses ''Board

-- | render a Board as a string
renderBoard :: (Show a) => Board a -> String
renderBoard (Board xs ys@[]) = show xs <> " " <> show ys
renderBoard (Board xs ys) = show xs <> "  " <> show ys

-- | A Board is good if the front stack is sorted in descending order.
good :: (Ord a) => Board a -> Bool
good (Board xs []) = and $ zipWith (<) <*> unsafeTail $ xs
good _ = False

-- | swap the first two elements of a list
swap :: [a] -> [a]
swap (x : y : xs) = y : x : xs
swap x = x

-- | swap the first two elements of the front stack
swapA :: Board a -> Board a
swapA = over front swap

-- | swap the first two elements of the back stack
swapB :: Board a -> Board a
swapB = over back swap

-- | push the first element of the front stack to the front of the back stack
pushAB :: Board a -> Board a
pushAB (Board (x : xs) ys) = Board xs (x : ys)
pushAB x = x

-- | push the first element of the back stack to the front of the front stack
pushBA :: Board a -> Board a
pushBA (Board xs (y : ys)) = Board (y : xs) ys
pushBA x = x

-- | rotate any list, moving the first element to the end
-- lists are not efficient for this, but it's easy to implement
-- please use finger trees instead
rotate :: [a] -> [a]
rotate (x : xs) = xs <> [x]
rotate [] = []

-- | rotate the front stack
rotateA :: Board a -> Board a
rotateA = over front rotate

-- | rotate the back stack
rotateB :: Board a -> Board a
rotateB = over back rotate

-- |  reverse rotate any list, moving the last element to the front
reverseRotate :: [a] -> [a]
reverseRotate [] = []
reverseRotate xs = unsafeLast xs : unsafeInit xs

reverseRotateA :: Board a -> Board a
reverseRotateA = over front reverseRotate

reverseRotateB :: Board a -> Board a
reverseRotateB = over back reverseRotate

-- | check if the the two first elements of the front stack are sorted
sortedA :: Ord a => Board a -> Bool
sortedA (Board (x : y : _) _) = x > y
sortedA _ = False

-- | check if the the two first elements of the back stack are sorted
sortedB :: Ord a => Board a -> Bool
sortedB (Board _ (x : y : _)) = x < y
sortedB _ = False

-- | decide which move to make, based on the sorted
pushOrSwap :: Ord a => Board a -> Board a
pushOrSwap x =
  if sortedA x
    then pushAB x
    else swapA x

-- | push all elements from the back stack to the front stack
pushAllToFront :: Board a -> Board a
pushAllToFront end@(Board _xs []) = end
pushAllToFront (Board xs ys) = pushAllToFront $ pushBA (Board xs ys)

-- | create a Board from a list
mkBoard :: [a] -> Board a
mkBoard xs = Board xs []

-- render a step in the Board
renderStep :: Show a => (Int, (String, Board a)) -> String
renderStep (i, (t, g)) = printf "%3d) %3s: %s" i t $ renderBoard g

solve :: [Integer] -> Maybe [(String, Board Integer)]
solve = bfs . mkBoard

report :: Maybe [(String, Board Integer)] -> IO ()
report = traverse_ $ traverse_ (putStrLn . renderStep) . zip [0 ..]

main :: IO ()
main = do
  traverse_
    ((>> putText "----") . report . solve)
    [ [3, 2 .. 1],
      [4, 3 .. 1],
      [5, 4 .. 1],
      [6, 5 .. 1],
      [3, 9, 10, 7, 1, 2]
    ]

steps :: [(String, Board a -> Board a)]
steps =
  [ ("pa", pushAB),
    ("pb", pushBA),
    ("sa", swapA),
    ("sb", swapB),
    ("ra", rotateA),
    ("rb", rotateB),
    ("rra", reverseRotateA),
    ("rrb", reverseRotateB),
    ("ss", swapA . swapB),
    ("rr", rotateA . rotateB),
    ("rrr", reverseRotateA . reverseRotateB)
  ]

-- | apply all possible moves to a Board
allMoves :: Board a -> [(String, Board a)]
allMoves x = map (\(t, f) -> (t, f x)) steps

-- | breadth first search
-- this is a flooding algorithm, it's guarded against loops
-- it has very primitive pruning
bfs :: Ord a => Board a -> Maybe [(String, Board a)]
bfs x = go mempty [[("", x)]]
  where
    go _ [] = Nothing
    go seen ((ty@(_, y) : ys) : xs)
      | good y = Just (reverse (ty : ys))
      | y `Set.member` seen = go seen xs
      | otherwise = go (Set.insert y seen) $ xs <> filter prune ((: ty : ys) <$> allMoves y)
    go _ _ = Nothing

identities :: Set (String, String)
identities =
  Set.fromList
    [ ("pa", "pb"),
      ("pb", "pa"),
      ("sa", "sa"),
      ("sb", "sb"),
      ("ra", "rra"),
      ("rra", "ra"),
      ("rb", "rrb"),
      ("rrb", "rb"),
      ("ss", "ss"),
      ("rr", "rr"),
      ("rrr", "rrr")
    ]

prune :: [(String, Board a)] -> Bool
prune ((tx, _) : (ty, _) : _) = (tx, ty) `Set.notMember` identities
prune _ = True