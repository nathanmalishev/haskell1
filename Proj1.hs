-- Project 1 Guessing


module Proj1 (initialGuess, nextGuess, GameState) where

    import Data.List
    data GameState = GameState { posPitch :: [[String]] }
                                


    initialGuess :: ([String],GameState)
    initialGuess = (["A1","A2","A3"], 
                (GameState allPossibleStates))

    nextGuess :: ([String],GameState) -> (Int,Int,Int) -> ([String],GameState)
    nextGuess ((xs_previous), (GameState posChord )) (pitch, note, octave) 
        = ((head posChord), GameState  newPosChords  )
        where newPosChords = [x | x<-posChord, ifTarget xs_previous x pitch note octave]
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
   

    -- makes sure there is no pitch that is the same
    validChord :: [String] -> Bool
    validChord [] = True
    validChord (x:xs)
        | x `elem` xs = False
        | otherwise = validChord xs


    -- remove all occurences of an element from list
    remove :: (Ord a) => a ->[a] -> [a]
    remove _ [] = []
    remove a (x:xs)
        | a == x = remove a xs
        | otherwise = x : remove a xs


    allPossibleStates:: [[String]]
    allPossibleStates = [[a]++[b]++[c] | a<- basicList, b<- basicList, c<- basicList, a/=b, a/=c, b/=c,c<a,b<a,b<c]
                where basicList = [b++a| a<-["1","2","3"], b<-["A","B","C","D","E","F","G"]]