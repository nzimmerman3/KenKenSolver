# KenKenSolver
Takes any unsolved KenKen puzzle and outputs all possible solutions

Project specification:
https://web.cs.ucla.edu/classes/winter18/cs131/hw/hw4.html

The predicate kenken(N, C, T) takes

N, a nonnegative integer specifying the number of cells on each side of the KenKen square.
C, a list of numeric cage constraints as described below.
T, a list of list of integers. All the lists have length N. This represents the N×N grid.
Each constraint in C is of the following form:

+(S, L)
means the integer S is the sum of integers in the list L of squares.
*(P, L)
means the integer P is the product of the integers in the list L of squares.
−(D, J, K)
means the integer D is the difference between the integer j in square J and the integer k in square K; D could be equal to either j−k or to k−j.
/(Q, J, K)
means the integer Q is the quotient of the integer j in square J and the integer k in square K; Q could be equal to either j÷k or to k÷j. The remainder must be zero.

In the above description, a square is a term [i|j] where i and j are row and column indexes in the range 1 through N, inclusive. The indexes identify the square in the KenKen diagram that is affected by the constraint in question. The bottom left square is [N|1] and the top right square is [1|N].
