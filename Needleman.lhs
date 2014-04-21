% 18mar2006  +chris+
\documentclass[12pt,fleqn]{article}

%let array = True

%include lhs2TeX.fmt
%include lhs2TeX.sty

% a visible space
\def\vissp{\texttt{'{\char'040}'}}
%format ' ' = "\vissp"

\usepackage[a4paper]{geometry}
\usepackage{hyperref}

\setlength{\parindent}{0pt}
\setlength{\parskip}{0.5ex}

\title{Dynamic Programming in Haskell}
\author{Christian Neukirchen\footnote{The author can be reached at
    \url{http://chneukirchen.org}.}}
\date{March 2006}

\begin{document}

\maketitle

The purpose of this Literate Haskell program is to implement a
function that does global sequence alignment using Needleman/Wunsch
techniques.\footnote{More about these techniques, graphics helpful
  for understanding, and a codeless step-by-step explanation can be
  found at
  \url{http://www.sbc.su.se/~pjk/molbioinfo2001/dynprog/dynamic.html}.}

The algorithm is based on two steps: first, filling a matrix with the
maximal alignment scores for each element and then tracing a path
connecting the top-left and the bottom-right cell.  Note that the
matrix is $O(n\cdot{}m)$ memory-wise and therefore pretty inefficient, you
don't want to use this on bigger sequences.

In Haskell, a good way to implement Dynamic Programming like this is
an array that will memoize a lazy stream of scores per cell.  This
allows $O(1)$-lookup of formerly calculated values without losing
referential transparency and (to an extent) lazy evaluation.

% hack, hack
\setlength{\mathindent}{2.5em}

\begin{code}
import Data.Array-0.4.0.1
\end{code}

|align| is the function that wraps all the functions below and calls
them in correct order.  It takes two strings and returns them aligned:

\begin{code}
align        :: String -> String -> [String]
align da db  = format $ reverse $ traceback lena lenb
    where
      lena = length da
      lenb = length db
\end{code}

% hack, hack
\setlength{\mathindent}{4.5em}

The algorithm is easier to express when the sequences to align are
one-indexed, since the borders of the matrix are used as special
values.  An easy way to achieve this is prepending a space:

\begin{code}
      a = ' ' : da
      b = ' ' : db
\end{code}

|memscore| is the array that contains the actual matrix.  It is filled
using a lazy stream of scores for each element.

\begin{code}
      memscore = listArray  ((0,0), (lena,lenb))
                            [score x y | x <- [0..lena], y <- [0..lenb]]
\end{code}

The scoring function looks very confusing since Haskell's array access
operator is not very elegant.  I'll introduce an infix operator $i$ |@@|
$j$ that corresponds to $M_{i,j}$:

\begin{code}
      infix 5 @@
      (@@) i j = memscore ! (i, j)
\end{code}

The score $M_{i,j}$ of each element is determined in below code as follows,
the borders of the matrix with $i=0$ and $j=0$ are initialized to zero.
(More complex scoring algorithms could be added easily.)

\[
M_{i,j} = \textrm{maximum of} \left\{%
\begin{array}{l}
M_{i-1,j-1} + S_{i,j} \\
M_{i,j-1} + w \\
M_{i-1,j} + w
\end{array}\right.
\]

The gap penalty $w$ is zero here for reasons of simplicity.

\begin{code}
      score 0 _ = 0
      score _ 0 = 0
      score x y = maximum [(  x-1  @@ y-1) + difference x y,
                              x-1  @@ y  ,
                              x    @@ y-1]
\end{code}

$S_{i,j}$ is a mismatch penalty defined here like this:

\[
S_{i,j} = \left\{%
\begin{array}{ll}
0 & \textrm{if the symbols at position $i$ and position $j$ match} \\
1 & \textrm{otherwise}
\end{array}\right.
\]

\begin{code}
        where difference x y  | a!!x == b!!y  = 1
                              | otherwise     = 0
\end{code}

|traceback| now finds the path connecting both corners of the matrix
and collects the appropriate symbols (or spaces for gaps).

\begin{code}
      traceback :: Int -> Int -> [(Char, Char)]
      traceback 0 0 = []
      traceback x y  | x     == 0       = (' '   , b!!y  ) : traceback  0      (y-1)
                     | y     == 0       = (a!!x  , ' '   ) : traceback  (x-1)  0
                     | x@@y  == x@@y-1  = (' '   , b!!y  ) : traceback  x      (y-1)
                     | x@@y  == x-1@@y  = (a!!x  , ' '   ) : traceback  (x-1)  y
                     | otherwise        = (a!!x  , b!!y  ) : traceback  (x-1)  (y-1)
\end{code}

The resulting list of tuples like |[('a', 'd'), ('b', 'e'), ('c', 'f')]|
gets converted by |format| into |["abc", "def"]|.
 
\begin{code}
      format l = [map fst l, map snd l]
\end{code}

Finally, a small main program to test the algorithm:
 
% hack, hack
\setlength{\mathindent}{2.5em}

\begin{code}
dna1 = "GAATTCAGTTA"
dna2 = "GGATCGA"

main = mapM_ putStrLn $ align dna1 dna2
\end{code}
%$
Expected output:

\begin{verbatim*}
G AATTCAGTTA
GGA T C G  A
\end{verbatim*}

As you can see, corresponding symbols are aligned, with appropriate
gaps in between.  Implementing more complex rules for alignment is
left as an exercise for the reader.

A run where bigger gaps are needed:

\begin{spec}
dna1 = "ATGGCTTCTACC"
dna2 = "TATCAAAAGCCG"
\end{spec}
\begin{verbatim*}
 ATGGCTTCTA    CC 
TAT  C    AAAAGCCG
\end{verbatim*}

\end{document}