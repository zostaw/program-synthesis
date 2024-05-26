module Main where

-- define grammar
data Expr 
        = Num Int
        | Mul Expr Expr
        | Add Expr Expr
        deriving Show

-- define evaluation rules
evalExpr :: Expr -> Int
evalExpr (Num n) = n
evalExpr (Mul n1 n2) = (evalExpr n1) * (evalExpr n2)
evalExpr (Add n1 n2) = (evalExpr n1) + (evalExpr n2)

-- declare and define equation
equation :: Expr
equation = (Add (Mul (Num 1) (Num 3)) (Mul (Add (Num 2) (Num 3)) (Num 5)))

main :: IO ()
main = do
    putStrLn "Evaluating:"
    print equation
    putStrLn "Output:"
    -- evaluate and print
    print (evalExpr equation)
