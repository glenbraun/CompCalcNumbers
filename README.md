# CompCalcNumbers
An answer to an assignment in the class "Introduction to the Design of Computational Calculi".

## Assignment
Design a calculus for the Arabic numbers with an Add operation. Extra credit to include Roman numerals.

## Answer
Let there be an ordered set of symbols where the first in the set represents zero. For example, in base 10 Arabic use "0123456789". Each subsequent member of the set is one greater than the previous.

### Counting up
Define a counting up (increase by one) operation where:
* Start from the right, reading the right most symbol
* Replace this symbol with the next greater symbol
* If there is no next greater symbol, replace the symbol with the first symbol and perform a carry, else stop.
* To carry, move left one character, and repeat counting up operation.

### Counting down
Define a counting down (decrease by one) operation where:
* Start from the right, reading the right most symbol
* Replace this symbol with the previous symbol
* If there is no next previous symbol, replace the symbol with the last symbol and borrow, else stop.
* To borrow, move left one character, and repeat counting down operation.

### Detecting Zero
A value with only one symbol and that symbol is the first symbol of the set has a value of zero, otherwise it is non-zero.

### Adding
Adding is defined in terms counting up the amount of the amount to add. 

V = the current value
Arg = the amount to add

Count up V, and count down Arg until Arg is zero.

## Implementation
The implementation supports positive and negative values, Add, Subtract, and Roman numerals, and base 2, 8, 10, and 16 of Arabic numerals.

