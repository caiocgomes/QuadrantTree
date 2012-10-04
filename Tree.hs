module Tree where

data Tree a = Branch {
                label :: a,
                subforest :: [Tree a]
                } deriving (Eq, Read, Show)

instance Monad Tree where
    return x = Branch x []
    (Branch x subtrees) >>= f = Branch y (subtrees1 ++ subtrees2)
                where (Branch y subtrees2)  = f x
                      subtrees1 = map (>>= f) subtrees

instance Functor Tree where
    fmap f (Branch x subtrees) = Branch (f x) (map (fmap f) subtrees)

