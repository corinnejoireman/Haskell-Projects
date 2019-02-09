{-Author: Rinn Joireman
project1.hs
problem 1: power-}

power :: Integer -> Integer -> Integer
power base 0 = 1
power base exponent
     |(Prelude.mod exponent 2) == 1 = base * power base (exponent - 1)
     |otherwise = (power base (div exponent 2))^2

{- problem 2: bouncy and tailBouncy -}

bouncy :: Float -> Float -> Integer -> Float
bouncy h i bounces = tailBouncy 0 h i bounces where
     tailBouncy :: Float -> Float -> Float -> Integer -> Float
     tailBouncy sum h i 0 = sum
     tailBouncy sum h i bounces = tailBouncy (sum + h + (h*i)) (h*i) i (bounces-1)

{-problem 3: newton, improve, goodEnough-}

goodEnough :: Float -> Float -> Float -> Bool
goodEnough num guess toVal
     |Prelude.abs (guess^2 - num) > toVal = False
     |otherwise = True

newton :: Float -> Float -> Float -> Float
newton num guess toVal
     |goodEnough num guess toVal = guess
     |otherwise = newton num (improve num guess) toVal where
           improve :: Float -> Float -> Float
           improve num guess = (guess + (num/guess))/2
