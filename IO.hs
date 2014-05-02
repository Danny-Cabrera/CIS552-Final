
--Module to read in FASTA formatted files

--Source: http://stackoverflow.com/questions/21802526/biohaskell-read-fasta-file

module IO where


import Bio.Sequence.Fasta
import Bio.Sequence.SeqData
import qualified Data.ByteString.Lazy.Char8 as LB
import Control.Monad.Error(ErrorT)
import Data.Foldable (mapM_)
import qualified Data.Map.Strict as Map
import Test.HUnit
import Align
import NeedlemanWunsch
import Phenotype



{-instance (Error e, MonadIO m) => MonadIO (ErrorT e m) where
   return x = -}

main :: IO () 
main = 	do 
  putStrLn "Input FASTA file:"
  file  <- getLine
  fasta <- readFasta file
  geneFile <- readFasta "phenotypes.fa"
  refFile <- readFasta "fastaRef.fa"
  let sequence = map castToNuc fasta :: [Sequence Nuc]
  let seqs = listify sequence 
  let genes = map castToNuc geneFile :: [Sequence Nuc]
  let geneMap = map listToGene genes
  let reference = map castToNuc refFile :: [Sequence Nuc]
  let refs = listify reference
  print $ genProfile (keyGenes (head refs) geneMap)



--Turn nucleotide sequence into a SeqData list
listify :: [Sequence Nuc] -> [SeqData]
listify (s:ss) = seqdata s : listify ss
listify _      = []





