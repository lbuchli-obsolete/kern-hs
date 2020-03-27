module Parser where

import Language
import Lexer
import Util

-- TODO? [Token] -> (a, [Token])
type Parser a = [Token] -> [(a, [Token])]

pOr :: Parser a -> Parser a -> Parser a
pOr p1 p2 tokens = p1 tokens ++ p2 tokens 

pThen :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
pThen combine p1 p2 tokens = [ (combine v1 v2, toks2) |
                               (v1, toks1) <- p1 tokens,
                               (v2, toks2) <- p2 toks1]

pThen3 :: (a -> b -> c -> d) -> Parser a -> Parser b -> Parser c -> Parser d
pThen3 combine p1 p2 p3 tokens = [ (combine v1 v2 v3, toks3) |
                                   (v1, toks1) <- p1 tokens,
                                   (v2, toks2) <- p2 toks1,
                                   (v3, toks3) <- p3 toks2]

-- TODO is there a way to generalize this?
pThen4 :: (a -> b -> c -> d -> e) -> Parser a -> Parser b -> Parser c -> Parser d -> Parser e
pThen4 combine p1 p2 p3 p4 tokens = [ (combine v1 v2 v3 v4, toks4) |
                                      (v1, toks1) <- p1 tokens,
                                      (v2, toks2) <- p2 toks1,
                                      (v3, toks3) <- p3 toks2,
                                      (v4, toks4) <- p4 toks3]

pZeroOrMore :: Parser a -> Parser [a]
pZeroOrMore p = pEmpty [] `pOr` pOneOrMore p

-- here, only the first parsing possibility is chosen, because the others follow from it
-- and are just wasted computing time
pOneOrMore :: Parser a -> Parser [a]
pOneOrMore p = take 1 . pThen (:) p (pZeroOrMore p)
 
pEmpty :: a -> Parser a
pEmpty a toks = [(a, toks)]

pApply :: Parser a -> (a -> b) -> Parser b
pApply p1 f toks = [(f a, rest) | (a, rest) <- p1 toks]

pOneOrMoreWithSep :: Parser a -> Parser b -> Parser [a]
pOneOrMoreWithSep p1 p2 = pThen (:) p1 (pThen (\_ x -> x) p2 (pOneOrMoreWithSep p1 p2) `pOr` pEmpty [])

pSat :: (String -> Bool) -> Parser String
pSat f (tok:toks) | f (third tok) = [(third tok, toks)]
pSat _ _ = [] 

pLit :: String -> Parser String
pLit s = pSat (== s)

pType :: TokenType -> Parser String
pType t (tok:toks) | second tok == t = [(third tok, toks)]
pType _ _ = []

pNum :: Parser Integer
pNum = pType KNumber `pApply` read

pVar :: Parser String
pVar = pType KVariable

------------------------------------------------------------------------
--                        Kern specific parsing                       --
------------------------------------------------------------------------

kParse :: [Token] -> KernProgram
kParse = take_first_parse . pProgram
  where
    take_first_parse ((prog, []) : _) = prog
    take_first_parse (_ : others)     = take_first_parse others
    take_first_parse _                = error "Syntax Error :("

pProgram :: Parser KernProgram
pProgram = pOneOrMoreWithSep pSc (pLit ";")

pSc :: Parser KernScDefn
pSc = pThen4 mkSc pVar (pZeroOrMore pVar) (pLit "=") pExpr

mkSc :: String -> [String] -> String -> KernExpr -> (Name, [Name], KernExpr)
mkSc name vars _ body = (name, vars, body)

pExpr :: Parser KernExpr
pExpr = pAExpr
  `pOr` pLambda
  `pOr` pCase
  `pOr` pLet
  `pOr` pThen EAp pExpr pAExpr

pAExpr :: Parser KernExpr
pAExpr = pThen3 (\_ x _ -> x) (pLit "(") pExpr (pLit ")")
  `pOr` pConst
  `pOr` (pNum `pApply` ENum)
  `pOr` (pVar `pApply` EVar)

pLambda :: Parser KernExpr
pLambda = pThen4 mkLam (pLit "\\") (pOneOrMore pVar) (pLit "->") pExpr

mkLam :: String -> [Name] -> String -> KernExpr -> KernExpr
mkLam _ vars _ = ELam vars

pCase :: Parser KernExpr
pCase = pThen4 mkCase (pLit "case") pExpr (pLit "of") (pOneOrMoreWithSep pAlter (pLit ";"))

mkCase :: String -> KernExpr -> String -> [KernAlt] -> KernExpr
mkCase _ expr _ = ECase expr

pAlter :: Parser KernAlt
pAlter = pThen4 mkAlter
  (pThen3 (\_ x _ -> x) (pLit "<") pNum (pLit ">"))
  (pZeroOrMore pVar)
  (pLit "=>")
  pExpr

mkAlter :: Integer -> [Name] -> String -> KernExpr -> KernAlt
mkAlter int vars _ body = (int, vars, body)

pLet :: Parser KernExpr
pLet = pThen4 mkLet
  ((pLit "let" `pOr` pLit "letrec") `pApply` (== "letrec"))
  (pOneOrMoreWithSep pDef (pLit ";"))
  (pLit "in")
  pExpr

mkLet :: Bool -> [(Name, KernExpr)] -> String -> KernExpr -> KernExpr
mkLet recursive defs _ = ELet recursive defs

pDef :: Parser (Name, KernExpr)
pDef = pThen3 (\name _ val -> (name, val)) pVar (pLit "=") pExpr

pConst :: Parser KernExpr
pConst = pThen4 (\_ _ x _ -> x)
  (pLit "Pack") (pLit "{")
  (pThen3 (\tag _ arity -> EConst tag arity) pNum (pLit ",") pNum)
  (pLit "}")
