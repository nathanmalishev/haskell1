-- Project 1 Guessing


module Proj1 (initialGuess, nextGuess, GameState) where

    data GameState = GameState { posPitch :: [[String]] }
                                


    initialGuess :: ([String],GameState)
    initialGuess = (["A1","A2","A3"], 
                (GameState allPossibleStates))

    nextGuess :: ([String],GameState) -> (Int,Int,Int) -> ([String],GameState)
    nextGuess ((xs_previous), (GameState defpitch )) (pitch, note, octave) 
        = ((head defpitch), GameState (tail defpitch))
        


    --allPossibleStates :: [String]
   

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



    allPossibleStates = [[a]++[b]++[c] | a<- basicList, b<- basicList, c<- basicList, a/=b, a/=c, b/=c,c<a,b<a,b<c]
                where basicList = [b++a| a<-["1","2","3"], b<-["A","B","C","D","E","F","G"]]