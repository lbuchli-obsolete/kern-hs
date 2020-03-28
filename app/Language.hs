module Language where

import Util

type Name = String

type Alter a = (Integer, [a], Expr a)
type KernAlt = Alter Name

data Expr a
  = EVar Name                -- Variables
  | ENum Integer             -- Numbers
  | EConst Integer Integer   -- Constructor (with tag and arity)
  | EAp (Expr a) (Expr a)    -- Applications
  | ELet                     -- Let(rec) expressions
    Bool                     -- Boolean, true = recursive, false = not recursive
    [(a, Expr a)]            -- Definitions
    (Expr a)                 -- Body
  | ECase                    -- Case expression
    (Expr a)                 -- Expression to scrutinise
    [Alter a]                -- Alternatives
  | ELam [a] (Expr a)        -- Lambda abstractions
  deriving Show

type KernExpr = Expr Name

type ScDefn a = (Name, [a], Expr a)
type KernScDefn = ScDefn Name

type Program a = [ScDefn a]
type KernProgram = Program Name

-- list of variables bound by definitions
bindersOf :: [(a, b)] -> [a]
bindersOf defns = [name | (name, _) <- defns]

-- list of right hand sides of definitions
rhssOf :: [(a, b)] -> [b]
rhssOf defns = [rhs | (_, rhs) <- defns]

-- returns true if the expression contains no internal structure, i.e. is atomic
isAtomicExpr :: Expr a -> Bool
isAtomicExpr (EVar _) = True
isAtomicExpr (ENum _) = True
isAtomicExpr _        = False

preludeDefs :: KernProgram
preludeDefs = [
  ("ident", ["x"], EVar "x"),
  ("fst", ["x", "y"], EVar "x"),
  ("snd", ["x", "y"], EVar "y"),
  ("s", ["f", "g", "x"], EAp (EAp (EVar "f") (EVar "x")) (EAp (EVar "g") (EVar "x"))),
  ("compose", ["f", "g", "x"], EAp (EVar "f") (EAp (EVar "g") (EVar "x"))),
  ("twice", ["f"], EAp (EAp (EVar "compose") (EVar "f")) (EVar "f"))
              ]


-- Pretty-print a Kern expression
-- This is a drastically simplified version of the one shown in the book
pprint :: KernExpr -> String
pprint (EVar n) = n
pprint (ENum i) = show i
pprint (EConst t a) = "Pack{" ++ show t ++ ", " ++ show a ++ "}"
pprint (EAp a b) = "(" ++ pprint a ++ " " ++ pprint b ++ ")"
pprint (ELet isrec defs body) = if isrec then "letrec" else "let" ++
  foldl (\prev x -> prev ++ ", " ++ x) " " (map (\x -> fst x ++ " " ++ pprint (snd x)) defs) ++
  " in " ++ pprint body 
pprint (ECase _ _) = "Not implemented I'm lazy"
pprint (ELam args body) = foldl (\prev x -> prev ++ " " ++ x) "\\" args ++
  " -> " ++ pprint body

-- Pretty-print a Kern supercombinator
pprintSc :: KernScDefn -> String
pprintSc sc = first sc ++ " " ++
  foldl (\prev x -> prev ++ " " ++ x) " " (second sc) ++
  " = " ++ pprint (third sc)

-- Pretty-print a Kern program
pprintProg :: KernProgram -> String
pprintProg = foldl (\prev x -> prev ++ "\n" ++ pprintSc x) ""
