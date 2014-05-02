
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
import System.Directory


{-instance (Error e, MonadIO m) => MonadIO (ErrorT e m) where
   return x = -}

main :: IO () 
main = 	do 
  putStrLn "Input FASTA file:"
  file  <- getLine
  do ex <- doesFileExist file
     if not ex then putStrLn "Invalid file name"
     else putStrLn "Opening File"
  -- case return (doesFileExist file) of
  -- 	 False -> putStrLn "File not found. Please try again."
  -- 	 True  -> 
  fasta    <- readFasta file
  putStrLn "Enter name: "
  name     <- getLine
  geneFile <- readFasta "phenotypes.fa"
  refFile  <- readFasta "fastaRef.fa"
  let sequence  = map castToNuc fasta :: [Sequence Nuc]
  let seqs      = listify sequence 
  let genes     = map castToNuc geneFile :: [Sequence Nuc]
  let geneMap   = map listToGene genes
  let reference = map castToNuc refFile :: [Sequence Nuc]
  let refs      = listify reference
  let generated = (generateGenome seqs (head refs))
  let prof      = genProfile (keyGenes generated geneMap)
  let out       = makeSeq prof
  print $ generateScore generated (head refs)
  writeFasta (name ++ "'s Profile.fa") out

--Create a Sequence from the SeqData for the output FASTA file
makeSeq :: [((SeqData, SeqData), String)] -> [Sequence a]
makeSeq (x:xs) = appendHeader (Seq (fst (fst x)) 
                              (snd (fst x)) 
                              Nothing) (snd x) : makeSeq xs
makeSeq _      = [] 


--Turn nucleotide sequence into a SeqData list
listify :: [Sequence Nuc] -> [SeqData]
listify (s:ss) = seqdata s : listify ss
listify _      = []





