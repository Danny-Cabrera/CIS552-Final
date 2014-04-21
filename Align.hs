
--implemenation of Needleman-Wunsch algorithm for global alignment



module Align where

import Data.Set
import Bio.Sequence
import TestData
import Test.HUnit

readLength :: Int
readLength = 5

emptyS :: Set (SeqData, Int)
emptyS = empty

indexGenome :: SeqData -> Set (SeqData, Int) -> Int -> Set (SeqData, Int) 
indexGenome seq set index = indexGenome (dropSeq readLength seq) (insert (takeSeq readLength seq, index) set) (index + 1) 


dropSeq :: Int -> SeqData -> SeqData
dropSeq i s = fromStr $ drop i (toStr s)

takeSeq :: Int -> SeqData -> SeqData
takeSeq i s = fromStr $ take i (toStr s)


test1 :: Test
test1 =  indexGenome refSeqShort empty 0 ~?= insert (fromStr "TTGGC", 2) (insert (fromStr "TCAAG", 1) (insert (fromStr "ACTGG", 0) empty))

splitRef :: Set (SeqData, Int)
splitRef = indexGenome refSeq empty 0
