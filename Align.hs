
--implemenation of Needleman-Wunsch algorithm for global alignment



module Align where

import Data.Set
import Bio.Sequence
import Bio.Alignment.SAlign
import TestData
import Test.HUnit
import Main
import Data.Array


readLength :: Int
readLength = 5


--Make sequence data of length multiple of readLength
padSeq :: SeqData -> SeqData
padSeq s = let q = lengthSeq s `mod` readLength in
			   if q == 0 then s else
               s `appendSeq` (buildSeq $ readLength - q)  

appendSeq :: SeqData -> SeqData -> SeqData
appendSeq s1 s2 = fromStr $ toStr s1 ++ toStr s2

buildSeq :: Int -> SeqData
buildSeq 0 = fromStr ""
buildSeq n = fromStr "T" `appendSeq` (buildSeq $ n - 1)

lengthSeq :: SeqData -> Int
lengthSeq s = length (toStr s)

indexer :: SeqData -> [(SeqData, Int)]
indexer s = indexGenome (padSeq s) [] 0

indexGenome :: SeqData -> [(SeqData, Int)] -> Int -> [(SeqData, Int)] 
indexGenome seq l index = if lengthSeq seq == 0 then l else
							indexGenome (dropSeq readLength seq) ((takeSeq readLength seq, index): l) (index + 1) 


dropSeq :: Int -> SeqData -> SeqData
dropSeq i s = fromStr $ drop i (toStr s)

takeSeq :: Int -> SeqData -> SeqData
takeSeq i s = fromStr $ take i (toStr s)


test1 :: Test
test1 =  indexer refSeqShort ~?= [(fromStr "TTGGC", 2), (fromStr "TCAAG", 1), (fromStr "ACTGG", 0)]

splitRef :: [(SeqData, Int)]
splitRef = indexGenome refSeq [] 0

--Needleman Wunsch Algorithm for scoring
show align seq1 refSeqShort

