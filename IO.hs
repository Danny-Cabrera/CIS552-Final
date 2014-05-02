
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



{-instance (Error e, MonadIO m) => MonadIO (ErrorT e m) where
   return x = -}

main :: IO () 
main = 	do 
  putStrLn "Input FASTA file:"
  file  <- getLine
  fasta <- readFasta file
  let sequence = map castToNuc fasta :: [Sequence Nuc]
  let seqs = listify sequence 
  print $ map toStr seqs
    --Data.Foldable.mapM_ print $ Map.lookup (fromStr "SEQUENCE_3") seqNameMap

--Function to map over FASTA file and translate it into a list of 
--tuples of the form: (Sequence Name, Sequence)	
listToSeq :: Sequence Nuc -> (SeqData, SeqData)
listToSeq s = (seqheader s, seqdata s)

--Turn nucleotide sequence into a SeqData list
listify :: [Sequence Nuc] -> [SeqData]
listify (s:ss) = seqdata s : listify ss
listify _      = []



