class Fluffy f where
  furry :: (a -> b) -> f a -> f b

-- Exercise 1
-- Relative Difficulty: 1
instance Fluffy [] where
  furry = map

-- Exercise 2
-- Relative Difficulty: 1
instance Fluffy Maybe where
  furry f x = x >>= return . f

-- Exercise 3
-- Relative Difficulty: 5
instance Fluffy ((->) t) where
  furry f g = f . g

newtype EitherLeft b a = EitherLeft (Either a b)
newtype EitherRight a b = EitherRight (Either a b)

-- Exercise 4
-- Relative Difficulty: 5
instance Fluffy (EitherLeft t) where
    -- furry :: (a -> b) -> EitherLeft t a -> EitherLeft t b
    furry f (EitherLeft (Left x))  = EitherLeft (Left (f x))
    furry _ (EitherLeft (Right x)) = EitherLeft (Right x)

-- Exercise 5
-- Relative Difficulty: 5
instance Fluffy (EitherRight t) where
    -- furry :: (a -> b) -> EitherRight t a -> EitherRight t b
    furry f (EitherRight (Right x)) = EitherRight (Right (f x))
    furry _ (EitherRight (Left x))  = EitherRight (Left x)

class Misty m where
    banana :: (a -> m b) -> m a -> m b
    unicorn :: a -> m a
               
    -- Exercise 6
    -- Relative Difficulty: 3
    -- (use banana and/or unicorn)
    furry' :: (a -> b) -> m a -> m b
    furry' f x = banana (unicorn . f) x

-- Exercise 7
-- Relative Difficulty: 2
instance Misty [] where
    banana f = concat . map f
    unicorn x = [x]

-- Exercise 8
-- Relative Difficulty: 2
instance Misty Maybe where
    banana f (Just m) = f m
    banana _ Nothing  = Nothing
    unicorn x = Just x

-- Exercise 9
-- Relative Difficulty: 6
instance Misty ((->) t) where
    banana f x c = f (x c) c
    unicorn = const

-- Exercise 10
-- Relative Difficulty: 6
instance Misty (EitherLeft t) where
    banana f (EitherLeft (Left x))  = f x
    banana _ (EitherLeft (Right x)) = EitherLeft (Right x)
    unicorn x = EitherLeft (Left x)

-- Exercise 11
-- Relative Difficulty: 6
instance Misty (EitherRight t) where
    banana f (EitherRight (Right x)) = f x
    banana _ (EitherRight (Left x))  = EitherRight (Left x)
    unicorn x = EitherRight (Right x)

-- Exercise 12
-- Relative Difficulty: 3
jellybean :: (Misty m) => m (m a) -> m a
jellybean = banana id

[a] -> [a ->b] -> [b]
Maybe a -> Maybe (a -> b) -> Maybe b

-- Exercise 13
-- Relative Difficulty: 6
apple :: (Misty m) => m a -> m (a -> b) -> m b
apple x f = 

-- Exercise 14
-- Relative Difficulty: 6
moppy :: (Misty m) => [a] -> (a -> m b) -> m [b]
moppy = error "todo"

-- Exercise 15
-- Relative Difficulty: 6
-- (bonus: use moppy)
sausage :: (Misty m) => [m a] -> m [a]
sausage = error "todo"

-- Exercise 16
-- Relative Difficulty: 6
-- (bonus: use apple + furry')
banana2 :: (Misty m) => (a -> b -> c) -> m a -> m b -> m c
banana2 = error "todo"

-- Exercise 17
-- Relative Difficulty: 6
-- (bonus: use apple + banana2)
banana3 :: (Misty m) => (a -> b -> c -> d) -> m a -> m b -> m c -> m d
banana3 = error "todo"

-- Exercise 18
-- Relative Difficulty: 6
-- (bonus: use apple + banana3)
banana4 :: (Misty m) => (a -> b -> c -> d -> e) -> m a -> m b -> m c -> m d -> m e
banana4 = error "todo"

newtype State s a = State {
  state :: (s -> (s, a))
}

-- Exercise 19
-- Relative Difficulty: 9
instance Fluffy (State s) where
  furry = error "todo"

-- Exercise 20
-- Relative Difficulty: 10
instance Misty (State s) where
  banana = error "todo"
  unicorn = error "todo"
