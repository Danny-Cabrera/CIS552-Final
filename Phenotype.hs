--
module Phenotype where

import Bio.Sequence.SeqData
import Bio.Sequence.Fasta
import Data.Foldable (mapM_)
import qualified Data.Map.Strict as Map
import Data.List
import qualified Data.ByteString.Lazy.Char8 as LB


--Identifies the specific gene sequences within the aligned sequence
--Call keyGenes with newly-aligned sequence
keyGenes :: SeqData -> [(SeqData, SeqData)] -> [(SeqData, SeqData)]
keyGenes s (x:xs) = if toStr (snd x) `isInfixOf` toStr s then (fst x, snd x) : keyGenes s xs
	                else keyGenes s xs
keyGenes _ _      = [] 	

--Generates a phenotypic profile with explanations based on genetic sequence
genProfile :: [(SeqData, SeqData)] -> [((SeqData, SeqData), String)]
genProfile (x:xs) = case toStr $ fst x of
	                   "ASPARAGUS_GENE_YES" -> (x, ": Gene for asparagus detection.") : genProfile xs
	                   "ASPARAGUS_GENE_NO" -> (x, ": No gene for asparagus detection.") : genProfile xs
	                   "EYE_COLOR_BROWN" -> (x, ": Gene for brown eyes.") : genProfile xs
	                   "EYE_COLOR_BLUE" -> (x, ": Gene for blue eyes.") : genProfile xs
	                   "EYE_COLOR_GRAY" -> (x, ": Gene for gray eyes.") : genProfile xs
	                   "HASKELL_GENE_BEGINNER" -> (x, ": You're a beginner in Haskell!") : genProfile xs
	                   "HASKELL_GENE_INTERMED" -> (x, ": You're an intermediate in Haskell!") : genProfile xs
	                   "HASKELL_GENE_EXPERT" -> (x, ": You're an expert in Haskell!") : genProfile xs
	                   "HEIGHT_GENE_TALL" -> (x, ": Gene for tallness.") : genProfile xs
	                   "HEIGHT_GENE_SHORT" -> (x, ": Gene for shortness.") : genProfile xs
genProfile _      = []

--Function to map over FASTA file and translate it into a list of 
--tuples of the form: (Gene Name, Sequence)	
listToGene :: Sequence Nuc -> (SeqData, SeqData)
listToGene s = (seqheader s, seqdata s)



