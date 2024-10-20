# Program Synthesis

My learning playground for Program Synthesis.

## First Notes

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

## Update 26.05.2024

I'm moving Haskell back. I think it's helpful to implement same thing in multiple languages to understand what it does. So, I'm planning to do so below in Haskell, Rust and maybe Rosette (it looks like a beautiful language).

The AST code in those languages can be represented like so:
1. Grammar - represents the possible operations - both in Haskell and Rust it can be implemented as Enum.  
2. Evaluation rules - operations that compose syntax tree based on grammar and an *equation* - usually implemented as a recursive function.  
3. The equation - an instance that is built from grammar in order to be evaluated.  

1. Implementation of Grammar can look as follows for those languages:

Let's assume single terminal of type *INT*.

**Haskell** - You create a *data* Enumeration type as below.

We define 3 options:
- Num - an actual value (Int), terminal
- Mul - non-terminal that uses 2 other AST terminals/non-terminals
- Add - same as above
- deriving Show - necessary for printing (evaluation)

```haskell
data Expr
         = Num Int
         | Mul Expr Expr
         | Add Expr Expr
         deriving Show
```

**Rust** - similarly, we define single terminal and 2 expressions

Maybe there is a better way to do it, but I found that necessary to use Boxes for each Expression, because Rust requires to declare memory up-front and that allows louzy allocation.  
Additionally, we need to implement Clone in order to evaluate recursively - you either need to create multiple pointers (Rc?) or just clone the objects. I found it easier to just clone them - it's more straight forward. Perhaps in larger programs, it might be wiser to use pointers, but I'm not familiar with Rc's. 

```rust
#[derive(Debug)]
enum Expr {
    Num(i32),
    Add(Box<Expr>, Box<Expr>),
    Mul(Box<Expr>, Box<Expr>),
}

impl Clone for Expr {
    fn clone(&self) -> Self {
        match self {
            Expr::Num(n) => Expr::Num(n.clone()),
            Expr::Add(lhs, rhs) => Expr::Add(lhs.clone(), rhs.clone()),
            Expr::Mul(lhs, rhs) => Expr::Mul(lhs.clone(), rhs.clone()),
        }
    }
}
```

2. Evaluation

In Haskel evaluation is pretty much straight forward:
- first you just define the evaluation function: take Expr and return Integer,
- then define evaluation rule-set that contains operations for each pattern from enum.

```haskell
evalExpr :: Expr -> Int
evalExpr (Num number) = number
evalExpr (Mul ast1 ast2) = (evalExpr ast1) * (evalExpr ast2)
evalExpr (Add ast1 ast2) = (evalExpr ast1) + (evalExpr ast2)
```

In Rust this part is really clean and elegant :)

```rust
fn eval_ast(expression: &Expr) -> i32 {
    match expression {
        Expr::Num(n) => n.clone(),
        Expr::Mul(n1, n2) => eval_ast(n1) * eval_ast(n2),
        Expr::Add(n1, n2) => eval_ast(n1) + eval_ast(n2),
    }
}
```


3. Equation

The equation to be evaluated.

**Haskell** - declare, define then evaluate the equation - pretty much straight forward

```haskell
equation :: Expr
equation = (Plus (Mult (Num 1) (Num 3)) (Mult (Plus (Num 2) (Num 3)) (Num 5)))
-- below can be executed directly in print statement
(evalExpr equation)
```

**Rust** - same

```rust
    let equation: Expr = Expr::Add(
        Box::new(Expr::Mul(Box::new(Expr::Num(1)), Box::new(Expr::Num(3)))),
        Box::new(Expr::Mul(
            Box::new(Expr::Add(Box::new(Expr::Num(2)), Box::new(Expr::Num(3)))),
            Box::new(Expr::Num(5)),
        )),
    );
    let result = eval_ast(&equation);
```

Both examples are stored in *haskell/ast/* and *rust/ast/*


## Update 24.06.2024

I was a little bit busy with work stuff over the last month and I did not have much time to continue learning synthesis, but I decided to focus on Racket programming language (parent language to Rosette), because it feels super powerfull for synthesis and any kind of manipulations. The language is expressive and potent, everything seems kind of effortless.  
So, I spent few days last month learning the basics and played a bit with Rosette, but realized that I need to start from the beginning, because symbolic synthesis is black-boxy to me when I tried to use it and I want to get to the bottom of it.  
So, last week I tried to write a synthesis program for words based on grammar structure (I chose some of Chomsky's ideas, because it's accessible and simple), but then I realized that although I can write the grammar, I don't really know how to generate sentences out of it. So, again I need to move back one more time. This weekend I dedicated myself to rewriting the same program from Armando's lectures, this time in Racket - to learn a bit of Racket and also to challenge myself, and also to cristalize my understanding of basic blocks of synthesis. I want to go through all methods - there doesn't seem to be as much information as you have about NNs for example. Bottom-up search is one that was popular few years ago, a lot changed, but I think it's important to understand it and also it's great because it's simple, so you can apply it to simple problems.  

I'm adding this implementation under [bottom-up-explicit-search.rkt](racket/bottom-up-explicit-search.rkt). It's not exactly the same as Rust - I did not include *Half*, because it adds a lot of search complexity.  


## Update 20.10.2024

I was doing a lot of racket stuff in another repository to learn FFI usage, because I feel like FFI is a necessity in Program Synthesis. So, I was mostly working on [llama-cpp-racket](https://github.com/zostaw/llama-cpp-racket) to enable myself to interface language models in Racket - tokenizers and even full inference. I learned a ton, but abandoned PS for some time. Then over some time now I've been learning RL stuff which I was planning to learn about even before. I started it before, but I just didn't understand the basic intuitions. After reading the PS lectures I saw MCMC explained to be really well and something clicked, so I went back to [The Bible](https://www.goodreads.com/book/show/39813875-reinforcement-learning) and have been working on this [project](https://github.com/zostaw/circle-expansion). Soo, that's about it, after I finish with it, I'm going to go back to PS. For now, I'm just leaving this important [Blog](https://blog.sigplan.org/2019/11/26/building-your-first-program-synthesizer/) for future reference.
