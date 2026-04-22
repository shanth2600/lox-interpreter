module Env where

import Lib
import qualified Data.Map as M

newtype Stack a = Stack [a]
  deriving Show

pop :: Stack a -> (Maybe a, Stack a)
pop (Stack [])    = (Nothing, Stack [])
pop (Stack (x:res)) = (Just x, Stack res)

push :: a -> Stack a -> Stack a
push a (Stack as) = Stack (a : as)

type Env k v = M.Map k (Stack v)

pushValue :: Ord k => k -> v -> Env k v -> Env k v
pushValue k v = M.insertWith (\_ st -> push v st) k (Stack [v])

peekValue :: Ord k => k -> Env k v -> Maybe v
peekValue k env =
  M.lookup k env >>= fst . pop

popValue :: Ord k => k -> Env k v -> Env k v
popValue = M.adjust (snd . pop)

popValues :: Ord k => [k] -> Env k v -> Env k v
popValues ks env = foldr popValue env ks

test = popValues ["x"] $ popValue "x" (pushValue "x" 2 (pushValue "x" 1 M.empty))