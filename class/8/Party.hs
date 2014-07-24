module Party where

import Employee
import Data.Monoid

instance Monoid GuestList where
    mempty = (GL [] 0)
    mappend (GL list fun) (GL list' fun') = (GL (list <> list') (fun+fun'))

glCons :: Employee -> GuestList -> GuestList
glCons emp (GL list score) = (GL (emp:list) (score+(empFun emp)))

moreFun :: GuestList -> GuestList -> GuestList
moreFun gl gl' =
    if (gl > gl') 
        then gl
        else gl'

treeFold :: (Monoid b) => (a -> b -> b) -> Tree a -> b
