-- Hutton’s Razor is a very simple expression language that expresses
-- integer literals and addition of values in that expression language.
-- The “trick” to it is that it’s recursive and the two expressions you’re
-- summing together could be literals or themselves further addition
-- operations. This sort of datatype is stereotypical of expression lan-
-- guages used to motivate ideas in research papers and functional
-- pearls. Evaluating or folding a datatype is also in some sense what
-- you’re doing most of the time while programming anyway.


-- Inspired from the paper: http://www.cs.nott.ac.uk/~pszgmh/bib.html#semantics

-- 1. Your first task is to write the “eval” function which reduces an
-- expression to a final sum.
data Expr
    = Lit Integer
    | Add Expr Expr

eval :: Expr -> Integer
eval (Lit i) = i
eval (Add x y) = eval x + eval y

-- 2. Write a printer for the expressions.
printExpr :: Expr -> String
printExpr (Lit i) = show i
printExpr (Add x y) = "(" ++ (printExpr x) ++ "+" ++ (printExpr y) ++ ")"
