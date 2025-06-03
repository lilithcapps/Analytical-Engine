{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE StandaloneDeriving     #-}
module Minecart  where
import           Data.Maybe (fromMaybe)

data Minecart c ps ns where
  Minecart ::  a -> [a] -> [a] -> Minecart a [a] [a]

deriving instance Show a => Show (Minecart a b c)

shorten :: Int -> Minecart a [a] [a] -> Maybe (Minecart a [a] [a])
shorten n (Minecart c ps ns) = Just (Minecart c (take n ps) (take n ns))

-- (<$>) :: (a -> b) -> Minecart a [a] [a] -> Minecart b [b] [b]
-- (<$>) f (Minecart c ps ns) = Minecart (f c) (fmap f ps) (fmap f ns)

class (MonadFail m) => MonadMinecart m where

  mount :: [a] -> m (Minecart a [a] [a])
  mount (a:as) = return (Minecart a [] as)
  mount []     = fail "no railz"

  dismount :: Minecart a [a] [a] -> m a
  dismount (Minecart a _ _) = return a

  transform :: (a -> b) -> Minecart a [a] [a] -> m (Minecart b [b] [b])
  transform f (Minecart c ps ns) = return (Minecart (f c) (fmap f ps) (fmap f ns))

  next :: Minecart a [a] [a] -> m (Minecart a [a] [a])
  next (Minecart c ps (n:ns)) = return (Minecart n (c:ps) ns)
  next (Minecart _ _ [])      = fail "outta railz"

  prev :: Minecart a [a] [a] -> m (Minecart a [a] [a])
  prev (Minecart c (p:ps) ns) = return (Minecart p ps (c:ns))
  prev (Minecart _ [] _)      = fail "outta railz"

  forwardsN :: Int -> Minecart a [a] [a] -> m (Minecart a [a] [a])
  forwardsN n mc
    | n > 0 = next mc >>= forwardsN (n-1)
    | otherwise = return mc

  backwardsN :: Int -> Minecart a [a] [a] -> m (Minecart a [a] [a])
  backwardsN n mc
    | n > 0 = prev mc >>= backwardsN (n-1)
    | otherwise = return mc

instance MonadMinecart Maybe

main :: IO ()
main = do
  let a = mount [0..] :: Maybe (Minecart Int [Int] [Int])
  let b = a >>= forwardsN 10
            >>= backwardsN 5
            >>= transform show
            >>= dismount

  putStrLn . fromMaybe "" $ b

  return ()
