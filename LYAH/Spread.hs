import Control.Applicative

data Spread a = Butter a | Margarine
    deriving Show

instance Functor Spread where
    fmap f (Butter a) = Butter (f a)
    fmap f Margarine = Margarine

instance Applicative Spread where
    pure a = Butter a
    Butter f <*> Butter x = Butter (f x)
    Margarine <*> _ = Margarine

instance Monad Spread where
    return a = Butter a
    Butter a >>= f = f a
    Margarine >>= _ = Margarine
    fail _ = Margarine

type Birds = Integer
type Pole = (Birds, Birds)

landRight :: Birds -> Pole -> Spread Pole
landRight n (left,right)
    | abs (left - (right + n)) < 4 = Butter (left, right + n)
    | otherwise = Margarine

landLeft :: Birds -> Pole -> Spread Pole
landLeft n (left,right)
    | abs (right - (left + n)) < 4 = Butter (left + n, right)
    | otherwise = Margarine
