module StateMonad where

newtype State s a = State { runState :: s -> (a,s) }

instance Functor (State s) where
    fmap f (State h) = State $ \s -> let (a, newState) = h s
                                         in (f a, newState) 

instance Applicative (State s) where
    pure x = State $ \s -> (x,s)
    -- Not sure what to do here yet -- 

instance Monad (State s) where
    return x = State $ \s -> (x,s)
    (State h) >>= f = State $ \s -> let (a, newState) = h s
                                        (State g) = f a
                                        in g newState
