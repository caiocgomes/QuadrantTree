import Tree

depth (Branch  _ []) = 0
depth (Branch  _ subtrees) = 1 + maximum (map depth subtrees)
