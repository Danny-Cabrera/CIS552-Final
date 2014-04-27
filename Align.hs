{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults -XDataKinds #-}
--implemenation of Needleman-Wunsch algorithm for global alignment

-- use flags for ghci: -package haskell98-2.0.0.2 -hide-packagebase

module Align where

import Bio.Sequence
import TestData
import Test.HUnit
import NeedlemanWunsch

readLength :: Int
readLength = 25


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
test1 =  indexer refSeq ~?= [(fromStr "TAGCTAGCTAGCTTCAGCTTTTTTT",3),(fromStr "AGTGATGGAAGACTCGAGGGTCTTT",2),(fromStr"GATCGATCGATCTTAAGGTGTGTAT",1),(fromStr "ACTGGTCAAGTTGGCCAATTGGCCA",0)]
splitRef :: [(SeqData, Int)]
splitRef = indexGenome refSeq [] 0

--Needleman Wunsch Algorithm for scoring
test2 :: Test
test2 = align (toStr seq1) (toStr refSeqShort) ~?= ["AGCTG", "GTCGATGGATCGACTAGGCTAGCAT"]

--Macth a read with it's bucket (returns similarity score of sequence with bucket)
match_read :: SeqData -> [(SeqData, Int)] -> Int -> [(Int,Int)]
--[([String], Int)]
match_read s (y:ys) curr_bucket = (match (align (toStr s) (toStr $ fst y)) 0, curr_bucket) : (match_read s ys (curr_bucket+1)) where
                                          match ((a:as):(b:bs):z) acc = 
                                            if a == b then 
                                              match (as:bs:z) (acc+1)
                                            else 
                                              match (as:bs:z) acc
                                          match _ acc = acc
match_read _ _ _ = []

--find max of list of tuples based on first element
max_fst :: Ord t => [(t, a)] -> (t, a)
max_fst (x:xs) = maxT x xs
  where maxT currMax [] = currMax
        maxT (a, b) (p:ps)
          | a < (fst p) = maxT p ps
          | otherwise   = maxT (a, b) ps
max_fst _     = error "no max on empty list"

--  match reads with buckets (takes a list of reads and an indexed reference genome, 
--	returns a list of reads with the bucket it matched to)
match_reads :: [SeqData]-> [(SeqData, Int)] -> [(SeqData,Int)]
match_reads (s:ss) r = (s, snd (max_fst (match_read s r 0))): (match_reads ss r)
match_reads _ _ = []
 

--needs to be implemented
test3 :: Test
test3 = match_reads ref_sequences (indexer refSeq) ~?= []

--given a list of reads, align each to reference
align_reads :: [SeqData] -> SeqData -> [String]
align_reads xs ref = filter (/= toStr ref) (redun xs ref) where
                        redun (l:ls) r = align (toStr l) (toStr r) ++ redun ls r
                        redun _ _      = []

test_align_reads :: Test
test_align_reads = align_reads ref_sequences refSeq ~?= []

--pairs each alignment with the corresponding genomic index
{-alignScores :: [SeqData] ->  [(SeqData, Int)] -> [([String], Int)]
alignScores (x:xs) (y:ys) = [(align (toStr x) (toStr $ fst y), snd y)] ++ alignScores xs ys
alignScores _ _ = [(["No sequences to align"], 0)] -}

