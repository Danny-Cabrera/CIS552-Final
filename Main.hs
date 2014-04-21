{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults -XDataKinds #-}
{-# LANGUAGE FlexibleInstances #-}

module TestData where

import Bio.Sequence



main :: IO ()
main = return ()


refSeq :: SeqData
refSeq = fromStr "ACTGGTCAAGTTGGCCAATTGGCCAGATCGATCGATCTTAAGGTGTGTATAGTGATGGAAGACTCGAGGGTCTTTTAGCTAGCTAGCTTCAGCT"
seq1 :: SeqData
seq1 = fromStr "ACTGG"
seq1 :: SeqData
seq2 = fromStr "GGCCATT"
seq1 :: SeqData
seq3 = fromStr "GATCGAT"
seq1 :: SeqData
seq4 = fromStr "TCGATC"
seq1 :: SeqData
seq5 = fromStr "GCTAGCT"
seq1 :: SeqData
seq6 = fromStr "GAGGGT"
seq1 :: SeqData
seq7 = fromStr "TAGCTAG"
seq1 :: SeqData
seq8 = fromStr "CTTTT"
seq1 :: SeqData
seq9 = fromStr "TTGGCCAG"
seq1 :: SeqData
seq10 = fromStr "CAAGT"
seq1 :: SeqData
seq11 = fromStr "TCTTA"
seq1 :: SeqData
seq12 = fromStr "CTCTA"
seq1 :: SeqData
seq13 = fromStr "GTGA"
seq1 :: SeqData
seq14 = fromStr "TGATG"
seq1 :: SeqData
seq15 = fromStr "TGGAA"
seq16 :: SeqData
seq16 = fromStr "CCAAT"
seq17 :: SeqData
seq17 = fromStr "GTCAAG"
seq18 :: SeqData
seq18 = fromStr "GTGTG"
seq19 :: SeqData
seq19 = fromStr "CTCGA"
seq20 :: SeqData
seq20 = fromStr "TTCA"



sequences :: [SeqData]
sequences = [seq1, seq2, seq3, seq4, seq5, seq6, seq7, seq8, seq9, seq10, seq11, seq12, seq13, seq14, seq15, seq16, seq17, seq18, seq19, seq20]
