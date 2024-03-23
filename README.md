# Program Synthesis

My learning playground for Program Synthesis.

## Notes

I'm learning from [this introduction course](https://people.csail.mit.edu/asolar/SynthesisCourse/) and to be honest it's a bit too difficult for me to get a head around it and I do not understand how to write it in python.  
It makes sense to me that I should reduce the amount of complexity.  
Code examples in tutorial are either in Haskell or Java - both languages alien to me.  
At first, I thought I could just follow along and try to write it in Rust as it's closer to both Java and Haskell in some sense, so I could do it one way or another, but that didn't seem to be the case.  
In the end I decided to learn few basics about Haskell and follow along with the course to understand the important building blocks in [Haskell].  

I actually started with Haskell and understood what I was missing: when I first approached the problem using python notebook, I defined each Expression as function. But this is wrong, because the whole program must be first formed before the input values are propagated. And I understood that when using Haskell code where you define data structure that defines Expressions and eval function that recursively evaluates it.

So, ultimately I moved back to Rust and in src is implementation of Bottom-Up Explicit Search as shown in [Armando Solar-Lezama's Lectures](https://people.csail.mit.edu/asolar/SynthesisCourse/Lecture3.htm)

This directory is not clean, because I want to preserve some of the learning materials, so I can go back to it, as I'm still not fully understanding.  
Later I will clean it up and remove redundancy.  
For now, I have 2 different languages convoluted:
- python - Just basic search through functions, not really synthesis, just search.
- Rust - Implementation of AST and program synthesis with bottom-up search
(I moved Haskell code to another repo)
