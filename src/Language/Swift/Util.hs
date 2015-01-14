-- Taken from Microsoft's Bond
module Language.Swift.Util where

import Data.Monoid

sepEndBy :: (Monoid a, Eq a) => a -> (t -> a) -> [t] -> a
sepBeginBy :: (Monoid a, Eq a) => a -> (t -> a) -> [t] -> a
sepBy :: (Monoid a, Eq a) => a -> (t -> a) -> [t] -> a

sepEndBy _ _ [] = mempty
sepEndBy s f (x:xs) 
    | next == mempty = rest
    | otherwise = next <> s <> rest
        where
            next = f x
            rest = sepEndBy s f xs

sepBeginBy _ _ [] = mempty
sepBeginBy s f (x:xs)
    | next == mempty = rest 
    | otherwise = s <> next <> rest
    where
        next = f x
        rest = sepBeginBy s f xs

sepBy _ _ [] = mempty
sepBy s f (x:xs)
    | null xs = next
    | next == mempty = rest
    | otherwise = next <> sepBeginBy s f xs
        where
            next = f x
            rest = sepBy s f xs

optional :: (Monoid m) => (a -> m) -> Maybe a -> m
optional = maybe mempty

between l r m
    | m == mempty = mempty
    | otherwise = l <> m <> r

angles m = between "<" ">" m
brackets m = between "[" "]" m
braces m = between "{" "}" m
parens m = between "(" ")" m

