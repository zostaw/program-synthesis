# Program Synthesis

My learning playground for Program Synthesis.

## Notes

I'm learning from [this introduction course](https://people.csail.mit.edu/asolar/SynthesisCourse/) and to be honest it's a bit too difficult for me to get a head around it and I do not understand how to write it in python.  
It makes sense to me that I should reduce the amount of complexity.  
Code examples in tutorial are either in Haskell or Java - both languages alien to me.  
At first I thought I could just follow along and try to write it in Rust as it's closer to both Java and Haskell in some sense, so I could do it one way or another, but that didn't seem to be the case.  
In the end I decided to learn few basics about Haskell and follow along with the course to understand the important building blocks in [Haskell].  
So, there is a small code in app/ for Haskell implementation of AST.  
But, then ultimately I moved back to Rust and in src is implementation of Bottom-Up Explicit Search as shown in [Armando Solar-Lezama's Lectures](https://people.csail.mit.edu/asolar/SynthesisCourse/Lecture3.htm)

This directory is not clean, because I want to preserve some of the learning materials, so I can go back to it, as I'm still not fully understanding.  
Later I will clean it up and remove redundancy.  
For now I have 3 different languages convoluted:
- python - Just basic search through functions, not really synthesis, just search.
- Haskel - Implementation of AST without synthesis/search
- Rust - Implementation of AST and program synthesis with bottom-up search
