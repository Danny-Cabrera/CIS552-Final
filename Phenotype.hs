--
module Phenotype where

import Bio.Sequence.SeqData
import Bio.Sequence.Fasta
import Data.Foldable (mapM_)
import qualified Data.Map.Strict as Map
import Data.List
import qualified Data.ByteString.Lazy.Char8 as LB


{-data Token = 
	GeneSeq SeqData --gene sequences
	  deriving (Eq, Show)-}

--Identifies the specific gene sequences within the aligned sequence
keyGenes :: SeqData -> [(SeqData, SeqData)] -> [(SeqData, SeqData)]
keyGenes s (x:xs) = if toStr (snd x) `isInfixOf` toStr s then (fst x, snd x) : keyGenes s xs
	                else keyGenes s xs
keyGenes _ _      = [] 	

--Generates a phenotypic profile with explanations based on genetic sequence
genProfile :: [(SeqData, SeqData)] -> [((String, String), String)]
genProfile (x:xs) = let a = toStr $ fst x in
                    let b = toStr $ snd x in
                 	case a of
	                   "ASPARAGUS_GENE_YES" -> ((a,b), "Gene for asparagus detection.") : genProfile xs
	                   "ASPARAGUS_GENE_NO" -> ((a,b), "No gene for asparagus detection.") : genProfile xs
	                   "EYE_COLOR_BROWN" -> ((a,b), "Gene for brown eyes.") : genProfile xs
	                   "EYE_COLOR_BLUE" -> ((a,b), "Gene for blue eyes.") : genProfile xs
	                   "EYE_COLOR_GRAY" -> ((a,b), "Gene for gray eyes.") : genProfile xs
	                   "HASKELL_GENE_BEGINNER" -> ((a,b), "You're a beginner in Haskell!") : genProfile xs
	                   "HASKELL_GENE_INTERMED" -> ((a,b), "You're an intermediate in Haskell!") : genProfile xs
	                   "HASKELL_GENE_EXPERT" -> ((a,b), "You're an expert in Haskell!") : genProfile xs
	                   "HEIGHT_GENE_TALL" -> ((a,b), "Gene for tallness.") : genProfile xs
	                   "HEIGHT_GENE_SHORT" -> ((a,b), "Gene for shortness.") : genProfile xs
genProfile _      = []

--Function to map over FASTA file and translate it into a list of 
--tuples of the form: (Gene Name, Sequence)	
listToGene :: Sequence Nuc -> (SeqData, SeqData)
listToGene s = (seqheader s, seqdata s)



