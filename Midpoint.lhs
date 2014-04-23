Final Project Midpoint Report
=============================

     <ALEXANDRA GOLUB>, <agolub>
     <DANIEL CABRERA>, <dcabrera>

Please give us a brief status report on the progress of your project
by editing [this file](Midpoint.lhs) and submitting it through the [course web
page](https://fling.seas.upenn.edu/~cis552/cgi-bin/current/submit.cgi).  

Briefly (one or two paragraphs max) respond to the following questions...

1.  How many hours have you spent on your project?

~10 hrs

2.  What have you accomplished so far?

So far we have created a set of test files including a “reference genome” and individual DNA "reads" with the intention of simulating an actual sequencing run where we know the genome of the species whose DNA we are sequencing. We have extended Haskell’s Bio.Sequence library to include the DNA equivalents of String drop and take, and we have also implemented functionality to fragment large sequences into smaller indexed regions, pad sequences, and compute sequence length. We are currently using an existing implementation of Needleman Wunsch algorithm to align the smaller sequences to the larger reference genome.  

We have written some tests and verified that we can correctly align sequences.

3.  Have you run into any unexpected challenges?

Yes - we are currently using the implementation of the Needleman Wunsch alignment algorithm (http://en.wikipedia.org/wiki/Needleman%E2%80%93Wunsch_algorithm) from another source we found online. However, the implementation uses Arrays and is not very Haskell-y. At this point we think we will just try to re-implement the algorithm ourselves in order to make the code cleaner and more efficient. The algorithm uses dynamic programming and a matrix structure to calculate different "scores" for alignment. Any advice or suggestions as to the best way to approach this implementation would be greatly appreciated.

Alternatively, we could continue to expand on functionality and build a fully functioning sequence aligner. It would be interesting to compare the performance of our fully functioning aligner against existing C++ implementations.It would be interesting to see where our implementation loses out or outperforms existing implementations. 

4.  Any other comments?

I think that is all!
