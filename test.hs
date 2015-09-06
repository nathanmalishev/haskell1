import Control.Monad

import Data.List
import System.Environment
import System.Exit
import Proj1

allPossibleStates = [[a]++[b]++[c] | a<- basicList, b<- basicList, c<- basicList, a/=b, a/=c, b/=c,c<a,b<a,b<c]--[ x | x<-(replicateM 3 [b++a | a<- ["1"], b<-["A","B","C"]]), validChord x]
                where basicList = [b++a| a<-["1","2","3"], b<-["A","B","C","D","E","F","G"]]



--if target takes in previous guess and potentional guess and produces
    -- the pitch, note & octave results
ifTarget :: [String] -> [String] -> Int->Int->Int -> Bool
ifTarget xs_previous possible pitch note octave
    | (pitchComparison xs_previous possible == pitch) && (noteComparison xs_previous possible == note) 
        && (octaveComparison xs_previous possible == octave) = True
    | otherwise = False

    -- a function which takes in previous guess and target and returns its pitch hint
pitchComparison :: [String] -> [String] -> Int
pitchComparison [] _ = 0
pitchComparison (p:previous) target 
    | p `elem` target = 1 + pitchComparison previous target
    |otherwise = pitchComparison previous target

--function takes in two guesses and returns the note hint
noteComparison  :: [String] -> [String] -> Int
noteComparison [] _ = 0
noteComparison previous target = (length previous) 
            - (length $ deleteFirstsBy (eqNth 0) previous target) - pitchComparison previous target

--function that takes in guess & target and returns octave hint
octaveComparison  :: [String] -> [String] -> Int
octaveComparison [] _ = 0
octaveComparison previous target = (length previous) 
            - (length $ deleteFirstsBy (eqNth 1) previous target) - pitchComparison previous target


-- | eqNth n l1 l2 returns True iff element n of l1 is equal to 
--   element n of l2.
eqNth :: Eq a => Int -> [a] -> [a] -> Bool
eqNth n l1 l2 = (l1 !! n) == (l2 !! n)


--write a function that takes the guess & results and if something in all possible states would not compute
-- then its not the target




-- take in possible targets and return the target that will leave the smallest amount of targets

--remove all guesses that does not have at least one of these pitches
--Take in previous guess and filter down possibilities
removeGuesses :: [String] -> [[String]] -> [[String]]
removeGuesses [] _ = []
removeGuesses _ [] = []
removeGuesses xs (y:ys)
    | pitchsInGuess xs y = y: removeGuesses xs ys
    | otherwise = removeGuesses xs ys


--remove all occurences of a Note from the possibilities
removeNote :: String -> [[String]] -> [[String]]
removeNote note [] = []
removeNote note (x:xs)
    | noteInGuess note x = removeNote note xs
    | otherwise = x:removeNote note xs


-- is the Note in the guess
noteInGuess :: String -> [String] -> Bool
noteInGuess note [] = False
noteInGuess note (x:xs) = if note == ([x !! 0]) then True else noteInGuess note xs

-- Are any of the pitches in the
pitchsInGuess :: [String] -> [String] -> Bool
pitchsInGuess [] _ = False
pitchsInGuess (p:pitches) guess 
    | p `elem` guess = True
    | otherwise = pitchsInGuess pitches guess
