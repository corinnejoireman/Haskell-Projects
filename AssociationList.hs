{-
author: Rinn Joireman
AssociationList.hs

Problem 1: Expects a key, a default value, and an a-list as arguments.
If the key is present, returns the value associated with it.
Otherwise, returns the default value.-}

module AssociationList where

get :: Eq a => a -> b -> [(a, b)] -> b
get key defaultVal [] = defaultVal
get key defaultVal ((c,d) : xs)
     | key == c = d
     | otherwise = get key defaultVal xs

{-Problem 2: Expects a key and an a-list as arguments.
If the key is present, returns the list without the key/value pair.
Otherwise, returns the list.-}

pop :: Eq a => a -> [(a, b)] -> [(a,b)]
pop key [] = []
pop key ((c,d) : xs)
     | key == c = xs
     | otherwise = (c,d) : pop key xs

{-Problem 3: Expects a key, a value, and an a-list as arguments.
If the key is present, returns the list with the value replacing the value previously associated with the key.
Otherwise, returns the list with the new key/value pair.-}

put :: Eq a => a -> b -> [(a, b)] -> [(a,b)]
put key value [] = [(key,value)]
put key value ((c,d):xs)
     |c == key = (key, d) : xs
     |otherwise = (c,d) : put key value xs
