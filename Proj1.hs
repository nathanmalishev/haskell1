-- Project 1 Guessing


module Proj1 (initialGuess, nextGuess, GameState) where

    data GameState = GameState { defpitch :: [Pitch]
                                , posNote :: [String]
                                , posOctave :: [String]
                                } deriving (Show)

    data Pitch = Pitch { note :: Char
                        , octave :: Char
                        } deriving (Show)


    initialGuess :: ([String],GameState)
    initialGuess = (["A1","D1","G1"], 
                (GameState [] ["A","B","C","D","E","F","G"] ["1","2","3"]))

    nextGuess :: ([String],GameState) -> (Int,Int,Int) -> ([String],GameState)
    nextGuess ((xs_previous), (GameState defpitch posNote posOctave)) (pitch, note, octave) 
        | all_notes_correct 
            = 
                ( xs_previous -- my next guess
                , GameState 
                    [] 
                    [[prevNotes!!0],[prevNotes!!1],[prevNotes!!2]] 
                    [])
        | all_notes_wrong 
            = (xs_previous, GameState [] (remove ([prevNotes!!2]) (remove ([prevNotes!!1]) (remove ([prevNotes!!0]) posNote))) [])
        where all_notes_correct = note + pitch == 3
              all_notes_wrong = note + pitch == 0
              prevNotes = [xs_previous!!0!!0, xs_previous!!1!!0, xs_previous!!2!!0]
              prevOctaves = [xs_previous!!0!!1, xs_previous!!1!!1, xs_previous!!2!!1]
        


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