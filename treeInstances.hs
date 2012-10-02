import Control.Monad
import Data.Foldable
import Data.Monoid
import Prelude hiding (foldr, maximum, minimum, concat, concatMap, and, or, any, all, sum, product, elem, notElem)
import qualified Prelude

data Tree a = Branch a [Tree a] deriving (Eq)
data Quadrant = First | Second | Third | Fourth deriving(Eq)

instance (Show a) => Show (Tree a) where
    show = unlines . draw

instance Functor Tree where
    fmap f (Branch x subtrees) = Branch (f x) (map (fmap f) subtrees)

instance Monad Tree where
    return x = Branch x []
    (Branch x subtrees) >>= f = Branch y (subtrees1 ++ subtrees2)
                where (Branch y subtrees2)  = f x
                      subtrees1 = map (>>= f) subtrees

instance Foldable Tree where
    foldMap f (Branch x []) = f x
    foldMap f (Branch x subtrees) = (f x) `mappend` (Prelude.foldr mappend mempty $ map (foldMap f) subtrees)


-- Drawing Trees
draw :: (Show a) => Tree a -> [String]
draw (Branch contents []) = [show contents]
draw (Branch contents subtrees) = (show contents) : drawSubTrees subtrees

drawSubTrees :: Show a => [Tree a] -> [String]
drawSubTrees [] = []
drawSubTrees [t] = "|" : shift "`- " "   " (draw t)
drawSubTrees (t:ts) = "|" : shift "+- " "|  " (draw t) ++ drawSubTrees ts

shift :: [a] -> [a] -> [[a]] -> [[a]]
shift first other = zipWith (++) (first : repeat other)

