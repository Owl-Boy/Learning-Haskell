{- 
  myMap :: (a -> b) -> [a] -> [b]
  myMap f [] = []
  myMap f (x:xs) = f x : myMap f xs 
-}

{-
  myFold :: (a -> b -> b) -> b -> [a] -> b
  myFold _ _ [] = []
  myFold fn start (x:xs) = myFold fn (fn start x) xs
-}

-- Generalizing Map and Fold to Trees

data Tree a = Leaf a | Branch (Tree a) (Tree a) deriving (Show)

treeMap :: (a -> b) -> Tree a -> Tree b
treeMap fn (Leaf x) = Leaf (fn x)
treeMap fn (Branch left right) = Branch (treeMap fn left) (treeMap fn right)

treeFold :: (b -> b -> b) -> (a -> b) -> Tree a -> b
treeFold fbranch fleaf = g where
  g (Leaf x) = fleaf x
  g (Branch left right) = fbranch (g left) (g right)


-- Generalizing Map and Fold further

data Weird a b = First a
               | Second b
               | Third [(a, b)]
               | Fourth (Weird a b)

weirdMap :: (a -> c) -> (b -> d) -> Weird a b -> Weird c d
weirdMap fa fb = g 
  where
    g (First a) = First (fa a)
    g (Second b) = Second (fb b)
    g (Third z) = Third (map (\(a, b) -> (fa a, fb b)) z)
    g (Fourth x) = Fourth (g x)


weirdFold :: (a -> c) -> (b -> c) -> ([(a, b)] -> c) -> (c -> c) -> Weird a b -> c
weirdFold f1 f2 f3 f4 = g
  where
    g (First x) = f1 x
    g (Second y) = f2 y
    g (Third z) = f3 z
    g (Fourth w) = f4 (g w)
