module K1Tree

where

import Math.Statistics
import Tree
import DrawTree

sprout :: (Floating a, Ord a) => [a] -> Tree [a]
sprout []  = Branch [] []
sprout [x] = Branch [x] []
sprout xs  = let med = median xs in  Branch [med] [Branch (filter (>med) xs) [], Branch (filter (<med) xs) []]


fixPoint:: (Eq a) => (a -> a) -> a -> a
fixPoint f x  = if (x == x') then x else fixPoint f x'  where x' = f x

k1Tree :: (Floating a, Ord a) => [a] -> Tree [a]
k1Tree xs = fixPoint (>>= sprout) (Branch xs [])
