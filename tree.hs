import Math.Statistics
import Control.Monad.Mersenne.Random
import System.Random.Mersenne.Pure64
import Control.Monad
import Control.Parallel

data Tree a = Branch a [Tree a] deriving (Eq)
data Quadrant = First | Second | Third | Fourth deriving(Eq)

instance Monad Tree where
    return x = Branch x []
    (Branch x subtrees) >>= f = Branch y (subtrees1 ++ subtrees2)
                where (Branch y subtrees2)  = f x
                      subtrees1 = map (>>= f) subtrees

instance Functor Tree where
    fmap f (Branch x subtrees) = Branch (f x) (map (fmap f) subtrees)

instance (Show a) => Show (Tree a) where
    show = unlines . draw

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

depth (Branch  _ []) = 0
depth (Branch  _ subtrees) = 1 + maximum (map depth subtrees)

-- 1D Hash
tree1d :: Tree [Double]
tree1d = Branch [1,2,3,4,5,6,7,8] []

sprout1d :: (Floating a, Ord a) => [a] -> Tree [a]
sprout1d [] = Branch [] []
sprout1d [x] = Branch [x] []
sprout1d xs = let avg = median xs in  Branch [avg] [Branch (filter (>avg) xs) [], Branch (filter (<=avg) xs) []]

-- 2d Hash
quadrant :: (Ord a, Ord b) => (a, b) -> (a, b) -> Quadrant
quadrant (xref,yref) (x, y) | (x >= xref) && (y >= yref)   = First
                            | (x < xref)  && (y >= yref)   = Second
                            | (x < xref)  && (y < yref)   = Third
                            | (x >= xref) && (y < yref)   = Fourth

quadrants :: (Ord a, Ord b) => (a, b) -> [(a, b)] -> [Quadrant]
quadrants ref xs = map (quadrant ref) xs

median2d :: (Floating t, Floating t1, Ord t, Ord t1) => [(t, t1)] -> (t, t1)
median2d points = let (xs, ys) = unzip points in (median xs, median ys)


move2quadrant :: (a, Quadrant) -> [[a]] -> [[a]]
move2quadrant (point, quad) [quad1, quad2, quad3, quad4] | (quad == First)  = [point:quad1, quad2, quad3, quad4]
                                                         | (quad == Second) = [quad1, point:quad2, quad3, quad4]
                                                         | (quad == Third)  = [quad1, quad2, point:quad3, quad4]
                                                         | (quad == Fourth) = [quad1, quad2, quad3, point:quad4]


filterQuadrants :: (Ord b, Ord a) => (a, b) -> [(a, b)] -> [[(a, b)]]
filterQuadrants ref points = move (quadList ref points)
                                where move [] = [[],[],[],[]]
                                      move (taggedPoint : taggedPoints) = move2quadrant taggedPoint quads where quads = move taggedPoints
                                      quadList ref points = zip points $ quadrants ref points

tree2d :: Tree [(Double, Double)]
tree2d = Branch [(0.0,0.0), (0.0,2.3), (1.5,3.1), (5.53, 2.15), (-2.2345, 10.12)] []

sprout2d :: (Floating a, Floating b, Ord a, Ord b) =>   [(a, b)] -> Tree [(a, b)]
sprout2d [] = Branch [] []
sprout2d [x] = Branch [x] []
sprout2d points = let refPoint = median2d points
                      quads = filterQuadrants refPoint points
                  in Branch [refPoint] (map (flip Branch $ []) $ quads)


normal :: Rand (Double, Double)
normal = do [x,y] <- replicateM 2 getDouble
            let r = sqrt(-2*log x)
            let theta = 2 * pi * y
            return $ (r*cos(theta), r * sin(theta))

getTree 0 points = Branch points []
getTree n points = tree >>= sprout2d where tree = getTree (n-1) points

main = do pureMT <- newPureMT
          let points = evalRandom (replicateM 100 normal) pureMT
          let tree = getTree 15 points
          print $ tree

