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

So far we have created a test library for the reference DNA data along with individual DNA "reads". We have been using this for testing our indexing and alignment algorithms, which we have also implemented in a separate module.

3.  Have you run into any unexpected challenges?

Yes - we are currently using the implementation of the Needleman Wunsch alignment algorithm (http://en.wikipedia.org/wiki/Needleman%E2%80%93Wunsch_algorithm) from another source we found online. However, the implementation uses Arrays, which are in the Haskell98 package (and for some reason import Data.Array wasn't working). At this point we think we will just try to re-implement the algorithm ourselves, preferably with a more "Haskell-y" data structure. The algorithm uses dynamic programming and a matrix structure to calculate different "scores" for alignment. Any advice or suggestions as to the best way to approach this implementation would be greatly appreciated. Thanks!

4.  Any other comments?

I think that is all!