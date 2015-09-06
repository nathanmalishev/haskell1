-- Project 1 Guessing


module Proj1 (initialGuess, nextGuess, GameState) where

    data GameState = GameState { posPitch :: [[String]] }
                                
    
    initialGuess :: ([String],GameState)
    initialGuess = (["A1","A2","A3"], 
                (GameState allPossibleStates))

    nextGuess :: ([String],GameState) -> (Int,Int,Int) -> ([String],GameState)
    nextGuess ((xs_previous), (GameState defpitch )) (pitch, note, octave) 
        = ((head defpitch), GameState (tail defpitch))
        

    -- Returns all possible guesses, without double ups / sym
    allPossibleStates :: [[String]]
    allPossibleStates = [[a]++[b]++[c] | a<- basicList, b<- basicList, c<- basicList, a/=b, a/=c, b/=c,c<a,b<a,b<c]
                where basicList = [b++a| a<-["1","2","3"], b<-["A","B","C","D","E","F","G"]]