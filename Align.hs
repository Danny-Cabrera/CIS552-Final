
--implemenation of Needleman-Wunsch algorithm for global alignment



module Align where

import Data.Set
import Bio.Sequence
import Bio.Alignment.SAlign
import TestData
import Test.HUnit

readLength :: Int
readLength = 5


--Make sequence data of length multiple of readLength
padSeq :: SeqData -> SeqData
padSeq s = let q = lengthSeq s `mod` readLength in
               s `appendSeq` (buildSeq $ readLength - q)  

appendSeq :: SeqData -> SeqData -> SeqData
appendSeq s1 s2 = fromStr $ toStr s1 ++ toStr s2

buildSeq :: Int -> SeqData
buildSeq 0 = fromStr ""
buildSeq n = fromStr "T" `appendSeq` (buildSeq $ n - 1)

lengthSeq :: SeqData -> Int
lengthSeq s = length (toStr s)

indexGenome :: SeqData -> [(SeqData, Int)] -> Int -> [(SeqData, Int)] 
indexGenome seq l index = indexGenome (dropSeq readLength seq) ((takeSeq readLength seq, index): l) (index + 1) 


dropSeq :: Int -> SeqData -> SeqData
dropSeq i s = fromStr $ drop i (toStr s)

takeSeq :: Int -> SeqData -> SeqData
takeSeq i s = fromStr $ take i (toStr s)


test1 :: Test
test1 =  indexGenome refSeqShort [] 0 ~?= [(fromStr "TTGGC", 2), (fromStr "TCAAG", 1), (fromStr "ACTGG", 0)]

splitRef :: [(SeqData, Int)]
splitRef = indexGenome refSeq [] 0

--Needleman Wunsch Algorithm for scoring

--global_score :: (Num a, Ord a) => SubstMx t a -> (a, a) -> Sequence t -> Sequence t -> a
--global_score matrix  seq1

