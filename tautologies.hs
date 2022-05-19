import Data.List
import Data.Maybe

data Prop = Const Bool
            | Var Char
            | Not Prop
            | And Prop Prop
            | Or Prop Prop
        deriving Show

eval :: [(Char, Bool)] -> Prop -> Bool
eval _ (Const x) = x
eval l (Var x) = fromJust (lookup x l)
eval l (Not x) = not (eval l x)
eval l (Or x y) = eval l x || eval l y 
eval l (And x y) = eval l x && eval l y

-- Takes all possible variables from formula
variables :: Prop -> [Char]
variables (Const _) = []
variables (Var x) = [x]
variables (Not x) = nub (variables x)
variables (And x y) = nub (variables x ++ variables y)
variables (Or x y) = nub (variables x ++ variables y)

-- Generates truth table for n variables
table :: Int -> [[Bool]]
table 0 = [[]]
table n =   let xs = table (n - 1)
            in map (True:) xs ++ map (False:) xs

-- Creates all possible evaluations for the formula
evaluations :: Prop -> [[(Char, Bool)]]
evaluations x = let vars = variables x
                in map (zip vars) (table (length vars))

isTaut :: Prop -> Bool
isTaut x = and [eval l x | l <- evaluations x]