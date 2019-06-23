
# Lamb Calculus 
## the repository formally known as λcalc

This is a interpreter for Lambda Calculus built using Haskell.

## Basics

### Lambdas
Lamba terms are written using a starting character, a divider, then another term.
  
  Starting characters: l, \, λ
  
  Dividers: ->, .

  an example is (lx y. x) or (λx y -> x)

### Booleans
true  := (lx y. x)
    
false := (lx y. y)

### Pairs
pairs can be written like (a, b)
  
pairs are accessed using fst and snd.
  
fst (a, b) = a
  
snd (a, b) = b

### Arrays
Arrays are written like [x1, x2, x3]
  
the empty list [] is called nil
  
they are implemented as a linked list, so this is equivalent to (x1, (x2, (x3, [])))
  
null is a function that returns true when given the empty list []

### Numbers
Numbers are represented as functions.
  
0 := (lf x. x)
    
1 := (lf x. f x)
    
2 := (lf x. f (f x))

the number is equal to the number of fs in the expression of the Lambda term.

## File Overview

### LambdaCalc.hs
Where are things relating to applying and creating functions comes from

### LambdaBool.hs
Where constants for true and false are kept, as well as boolean functions like not, and, or

### LambdaNum.hs 
Where integers stored in Lambdas are kept. There is a function that perfectly turns a number into a Lambda function, however, converting back or doing math does not currently work properly. However, isZero works perfectly fine.
