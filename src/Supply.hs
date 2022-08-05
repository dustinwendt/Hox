module Supply where

data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons a s) = a : streamToList s

instance Show a => Show (Stream a) where
  show x = g $ take 20 (streamToList x)
           where g []     = ""
                 g [x]    = show x
                 g (x:xs) = show x ++ " " ++ g xs


streamRepeat :: a -> Stream a
streamRepeat a = Cons a (streamRepeat a)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons a s) = Cons (f a) (streamMap f s)

streamIterate :: (a -> a) -> a -> Stream a
streamIterate f i = Cons i (streamIterate f (f i))

streamInterleave :: Stream a -> Stream a -> Stream a
streamInterleave (Cons x xs) ys = Cons x (streamInterleave ys xs)

nats :: Stream Integer
nats = streamIterate (+1) 0

newtype Supply s a = S (Stream s -> (a, Stream s))

get :: Supply s s
get = S (\(Cons a s) -> (a, s))

pureSupply :: a -> Supply s a
pureSupply a = S (\s -> (a, s))

mapSupply :: (a -> b) -> Supply s a -> Supply s b
mapSupply f sup = f <$> sup

mapSupply2 :: (a -> b -> c) -> Supply s a -> Supply s b -> Supply s c
mapSupply2 f s1 s2 = do a <- s1
                        f a <$> s2

bindSupply :: Supply s a -> (a -> Supply s b) -> Supply s b
bindSupply sup f = do a <- sup
                      f a

runSupply :: Stream s -> Supply s a -> a
runSupply s (S sup) = fst $ sup s

instance Functor (Supply s) where
  fmap = mapSupply

instance Applicative (Supply s) where
  pure = pureSupply
  (<*>) = mapSupply2 id

instance Monad (Supply s) where
  return = pureSupply
  (>>=) = bindSupply

