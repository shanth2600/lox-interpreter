{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
module Env where


import Control.Monad.Except
import Prelude hiding (last, init)
import Data.List.NonEmpty
    ( NonEmpty(..), cons, fromList, toList, uncons, last, init)
import qualified Data.Map as M
import Text.Printf (printf)
import Data.Maybe (isNothing, isJust)
import Control.Applicative ((<|>))
import Data.Foldable (asum)


type Scope k v = M.Map k v

newtype Env k v = Env (NonEmpty (Scope k v))
  deriving Show

emptyScope :: Scope k v
emptyScope = M.empty

emptyEnv :: Env k v
emptyEnv = Env (emptyScope :| [])

globalScope :: Env k v -> Scope k v
globalScope (Env scopes) = last scopes

insertGlobalScope :: Scope k v -> Env k v -> Env k v
insertGlobalScope scope (Env scopes) = Env (fromList $ init scopes ++ [scope]) 

declareVariable :: forall k v. Ord k => k -> v -> Env k v -> Env k v
declareVariable k v (Env (scope :| rest)) =
  Env $ (M.insert k v scope) :| rest

declareVariables :: forall k v. Ord k => [(k,  v)] -> Env k v -> Env k v
declareVariables bnds env = foldr (uncurry declareVariable) env bnds

pushNewScope :: Scope k v -> Env k v -> Env k v
pushNewScope scope (Env scopes) = Env $ scope `cons` scopes

popScope :: Env k v -> Maybe (Env k v)
popScope (Env scopes) = Env <$> (snd $ uncons scopes)

assignToVariable :: forall k v. Ord k => k -> v -> Env k v -> Maybe (Env k v)
assignToVariable k v (Env scopes) = Env . fromList <$> go (toList scopes)
  where
    go :: [Scope k v] -> Maybe [Scope k v]
    go []              = Nothing
    go (scope:scopes')
      | M.member k scope = Just $ M.insert k v scope : scopes'
      | otherwise        = (scope :) <$> go scopes'

lookupVariable :: forall k v. Ord k => k -> Env k v -> Maybe v
lookupVariable k (Env scopes) = asum . map (M.lookup k) $ toList scopes