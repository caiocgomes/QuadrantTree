module DrawTree (draw)
where

import Tree

draw :: (Show a) => Tree a -> String
draw = unlines . draw'

draw' :: (Show a) => Tree a -> [String]
draw' (Branch contents []) = [show contents]
draw' (Branch contents subtrees) = (show contents) : drawSubTrees subtrees

drawSubTrees :: Show a => [Tree a] -> [String]
drawSubTrees [] = []
drawSubTrees [t] = "|" : shift "`- " "   " (draw' t)
drawSubTrees (t:ts) = "|" : shift "+- " "|  " (draw' t) ++ drawSubTrees ts

shift :: [a] -> [a] -> [[a]] -> [[a]]
shift first other = zipWith (++) (first : repeat other)

