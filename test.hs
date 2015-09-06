import Control.Monad

import Data.List
import System.Environment
import System.Exit
import Proj1

allPossibleStates = [[a]++[b]++[c] | a<- basicList, b<- basicList, c<- basicList, a/=b, a/=c, b/=c,c<a,b<a,b<c]--[ x | x<-(replicateM 3 [b++a | a<- ["1"], b<-["A","B","C"]]), validChord x]
                where basicList = [b++a| a<-["1","2"], b<-["A","B","C"]]

