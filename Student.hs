{-
File: Student.hs

Rinn Joireman

Represents a student with a name and a set of scores.
-}

module Student where

data Student = Student String [Int]
  deriving (Show)

newStudent :: String -> Int -> Student
newStudent name numberOfScores =
  let scores = map (\x -> 0) [1..numberOfScores]
  in Student name scores

-- Returns the number of scores for this student

{-getNumberOfScores ::Student -> Int
getNumberOfScores (Student name scores) = length scores-}

getNumberOfScores ::Student -> Int
getNumberOfScores (Student name scores) = tailScoreCount (Student name scores) 0 where
     tailScoreCount :: Student -> Int -> Int
     tailScoreCount (Student name []) sum = sum
     tailScoreCount(Student name (x:xs)) sum = tailScoreCount (Student name xs) (sum + 1)

getName :: Student -> String
getName (Student name _) = name

setName :: String -> Student -> Student
setName newName (Student _ scores) =
  Student newName scores

-- Returns the student's score at the given index,
-- counting from 0

getScore :: Int -> Student -> Int
getScore 0 (Student name (x:xs)) = x
getScore index (Student name (x:xs)) = getScore (index - 1) (Student name xs)

-- Resets the student's score at the given index,
-- counting from 0, and returns the student
setScore :: Int -> Int -> Student -> Student
setScore index newScore (Student name scores) = Student name (replace index newScore scores) where
     replace :: Int -> Int-> [Int]-> [Int]
     replace 0 newScore (x:xs) = newScore : xs
     replace index newScore (x:xs) = x : replace (index - 1) newScore xs

-- Returns the student's highest score
getHighScore :: Student -> Int
getHighScore (Student name [x]) = x
getHighScore (Student name (x:xs))
     |x >= (head xs) = getHighScore (Student name (x: (tail xs)))
     |otherwise = getHighScore (Student name xs)

-- Returns the average score of all the students scores
getAverageScore :: Student -> Float
getAverageScore (Student name scores) = fromIntegral (sum scores) / fromIntegral (length scores)
