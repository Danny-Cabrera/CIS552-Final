{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults -XDataKinds #-}
{-# LANGUAGE FlexibleInstances #-}

module TestData where

import Bio.Sequence

--Reference genome data

refSeq :: SeqData
refSeq = fromStr "ACTGGTCAAGTTGGCCAATTGGCCAGATCGATCGATCTTAAGGTGTGTATAGTGATGGAAGACTCGAGGGTCTTTTAGCTAGCTAGCTTCAGCT"

refSeqShort :: SeqData
refSeqShort = fromStr "ACTGGTCAAGTTGGC"

--Individual DNA "reads" no mutations
seq1 :: SeqData
seq1 = fromStr "ACTGGTCAAGTTGGC"
seq2 :: SeqData
seq2 = fromStr "TGGCCAATTGGCCAGATC"
seq3 :: SeqData
seq3 = fromStr "CAGATCGATCGATCTT"
seq4 :: SeqData
seq4 = fromStr "TCTTAAGGTGTGTATAGT"
seq5 :: SeqData
seq5 = fromStr "ATAGTGATGGAAGACTC"
seq6 :: SeqData
seq6 = fromStr "GACTCGAGGGTCTTT"
seq7 :: SeqData
seq7 = fromStr "CTTTTAGCTAGCTAGCTTCAGCT"
seq8 :: SeqData
seq8 = fromStr "AGCTTCAGCT"
seq9 :: SeqData
seq9 = fromStr "GGAAGACT"
seq10 :: SeqData
seq10 = fromStr "GATCGATCGAT"

-- with mutations
seq11 :: SeqData
seq11 = fromStr "ACTGGTCACGTTGGC"
seq12 :: SeqData
seq12 = fromStr "TGGCCAATTGGCCAGATA"
seq13 :: SeqData
seq13 = fromStr "CAGATCGATCGATCTT"
seq14 :: SeqData
seq14 = fromStr "TCTTAAGGAGTGTATAGT"
seq15 :: SeqData
seq15 = fromStr "ATAGTGATGGAAGACTC"
seq16 :: SeqData
seq16 = fromStr "GACTCGAGGGTCTTA"
seq17 :: SeqData
seq17 = fromStr "CTTTTAGCTAGCTAGCTTCAGCT"
seq18 :: SeqData
seq18 = fromStr "AGCTTCAGCA"
seq19 :: SeqData
seq19 = fromStr "GGAAGACT"
seq20 :: SeqData
seq20 = fromStr "CATCGATCGAT"


ref_sequences :: [SeqData]
ref_sequences = [seq1, seq2, seq3, seq4, seq5, seq6, seq7, seq8, seq9, seq10]

mut_sequences :: [SeqData]
mut_sequences = [seq11, seq12, seq13, seq14, seq15, seq16, seq17, seq18, seq19, seq20]
