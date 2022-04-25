data Maybe' a= Nil | Some a deriving Show

data Tree a = Leaf a | Branch (Tree a) (Tree a) deriving Show

instance Functor Maybe' where
  fmap f Nil = Nil
  fmap f (Some x) = Some (f x)

instance Functor Tree where
  fmap f (Leaf a) = Leaf (f a)
  fmap f (Branch l r) = Branch (fmap f l) (fmap f r)


