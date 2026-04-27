module Lib where

type LineNumber = Int
type Ident = String

-- class Pretty a where
--   pretty :: a -> String

tailMay :: [a] -> Maybe [a]
tailMay [] = Nothing
tailMay (_:xs) = Just xs