{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults -XDataKinds #-}
--implemenation of Needleman-Wunsch algorithm for global alignment

-- use flags for ghci: -package haskell98-2.0.0.2 -hide-packagebase

module Align where

import Bio.Sequence
import TestData
import Test.HUnit
import NeedlemanWunsch

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
indexGenome seqs l index = if lengthSeq seqs == 0 then l else
                          indexGenome (dropSeq readLength seqs) ((takeSeq readLength seqs, index): l) (index + 1) 


dropSeq :: Int -> SeqData -> SeqData
dropSeq i s = fromStr $ drop i (toStr s)

takeSeq :: Int -> SeqData -> SeqData
takeSeq i s = fromStr $ take i (toStr s)


test1 :: Test
test1 =  indexer refSeqShort ~?= [(fromStr "TTGGC", 2), (fromStr "TCAAG", 1), (fromStr "ACTGG", 0)]

splitRef :: [(SeqData, Int)]
splitRef = indexGenome refSeq [] 0

--Needleman Wunsch Algorithm for scoring
test2 :: Test
test2 = align (toStr seq1) (toStr refSeqShort) ~?= ["AGCTG", "GTCGATGGATCGACTAGGCTAGCAT"]

--Call: alignment sequences (indexer refSeq)
--pairs each alignment with the corresponding genomic index
alignment :: [SeqData] -> [(SeqData, Int)] -> [([String], Int)]
alignment (x:xs) (y:ys) = [(align (toStr x) (toStr $ fst y), snd y)] ++ alignment xs (y:ys)
alignment _ _           = [(["No sequences to align"], 0)]

test3 :: Test
test3 = alignment sequences (indexer refSeq) ~?= [(["AGCTG"], 15), (["AGCTG"], 13)]

--pairs each alignment with the corresponding genomic index
{-alignScores :: [SeqData] ->  [(SeqData, Int)] -> [([String], Int)]
alignScores (x:xs) (y:ys) = [(align (toStr x) (toStr $ fst y), snd y)] ++ alignScores xs ys
alignScores _ _ = [(["No sequences to align"], 0)] -}

