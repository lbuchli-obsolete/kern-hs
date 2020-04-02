module Parser where

import Language
import Lexer
import Util


type Parser a = [Token] -> ParseResult a

type ParseResult a = Either (a, [Token]) String

pOrElse :: Parser a -> Parser a -> Parser a
pOrElse p1 p2 tokens = orP2 (p1 tokens)
  where
    orP2 (Left a)  = Left a
    orP2 (Right _) = p2 tokens

pAnd :: ParseResult a -> Parser b -> ParseResult b
pAnd (Left (_, tokens)) p = p tokens
pAnd (Right msg) _        = Right msg

pThen :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
pThen combine p1 p2 tokens = res p1res p2res
  where
    p1res = p1 tokens
    p2res = pAnd p1res p2
    res (Left (a, _)) (Left (b, toks)) = Left (combine a b, toks)
    res (Right msg) _                  = Right msg
    res _ (Right msg)                  = Right msg

pThen3 :: (a -> b -> c -> d) -> Parser a -> Parser b -> Parser c -> Parser d
pThen3 combine p1 p2 p3 tokens = res p1res p2res p3res
  where
    p1res = p1 tokens
    p2res = pAnd p1res p2
    p3res = pAnd p2res p3
    res (Left (a, _)) (Left (b, _)) (Left (c, toks)) = Left (combine a b c, toks)
    res (Right msg) _ _                              = Right msg
    res _ (Right msg) _                              = Right msg
    res _ _ (Right msg)                              = Right msg

pThen4 :: (a -> b -> c -> d -> e) -> Parser a -> Parser b -> Parser c -> Parser d -> Parser e
pThen4 combine p1 p2 p3 p4 tokens = res p1res p2res p3res p4res
  where
    p1res = p1 tokens
    p2res = pAnd p1res p2
    p3res = pAnd p2res p3
    p4res = pAnd p3res p4
    res (Left (a, _)) (Left (b, _)) (Left (c, _)) (Left (d, toks)) = Left (combine a b c d, toks)
    res (Right msg) _ _ _                                          = Right msg
    res _ (Right msg) _ _                                          = Right msg
    res _ _ (Right msg) _                                          = Right msg
    res _ _ _ (Right msg)                                          = Right msg

pZeroOrMore :: Parser a -> Parser [a]
pZeroOrMore p = pOneOrMore p `pOrElse` pEmpty [] 

pOneOrMore :: Parser a -> Parser [a]
pOneOrMore p = pThen (:) p (pZeroOrMore p)
 
pEmpty :: a -> Parser a
pEmpty a toks = Left (a, toks)

pApply :: Parser a -> (a -> b) -> Parser b
pApply p1 f toks = mapLeft (\x -> (f (fst x), snd x)) p1res
  where
    p1res = p1 toks

pOneOrMoreWithSep :: Parser a -> Parser b -> Parser [a]
pOneOrMoreWithSep p1 p2 = pThen (:) p1 (pThen (\_ x -> x) p2 (pOneOrMoreWithSep p1 p2) `pOrElse` pEmpty [])

pSat :: (String -> Bool) -> Parser String
pSat f (tok:toks) | f (third tok) = Left (third tok, toks)
pSat _ _                          = Right "Token does not satisfy property"

-- Implementing this with pSat would be easier, but does not lead to very useful error messages
-- and thus is implemented from scratch
pLit :: String -> Parser String
pLit s (tok:toks) | s == third tok = Left (third tok, toks)
pLit s _                           = Right ("Expected '" ++ s ++ "'")

pNotOf :: [String] -> Parser String
pNotOf blacklist (tok:toks) | third tok `notElem` blacklist = Left (third tok, toks)
pNotOf blacklist _                                          = Right ("Token is one of " ++ show blacklist)

pType :: TokenType -> Parser String
pType t (tok:toks) | second tok == t = Left (third tok, toks)
pType t _                            = Right ("Expected token of type " ++ show t)

pNum :: Parser Integer
pNum = pType KNumber `pApply` read

pVar :: Parser String
pVar toks = both (pType KVariable toks) (pNotOf keywords toks)
  where
    both (Left (res, tokens)) (Left _) = Left (res, tokens)
    both _ _                           = Right "Expected a variable"

------------------------------------------------------------------------
--                        Kern specific parsing                       --
------------------------------------------------------------------------

kParse :: [Token] -> Either KernProgram String
kParse toks = mapLeft fst (pProgram toks)

pProgram :: Parser KernProgram
pProgram = pOneOrMoreWithSep pSc (pLit ";")

pSc :: Parser KernScDefn
pSc = pThen4 mkSc pVar (pZeroOrMore pVar) (pLit "=") pExpr

mkSc :: String -> [String] -> String -> KernExpr -> (Name, [Name], KernExpr)
mkSc name vars _ body = (name, vars, body)

pExpr :: Parser KernExpr
pExpr =     pLambda
  `pOrElse` pCase
  `pOrElse` pLet
  `pOrElse` pInfixExpr
  `pOrElse` pThen EAp pAExpr pExpr
  `pOrElse` pAExpr

pAExpr :: Parser KernExpr
pAExpr = pThen3 (\_ x _ -> x) (pLit "(") pExpr (pLit ")")
  `pOrElse` pConst
  `pOrElse` (pNum `pApply` ENum)
  `pOrElse` (pVar `pApply` EVar)

-- TODO precedence
pInfixExpr :: Parser KernExpr
pInfixExpr = pThen3 (\a o b -> EAp (EAp (EVar o) a) b) pAExpr
  (pLit "*" `pOrElse` pLit "/" `pOrElse` pLit "+" `pOrElse` pLit "-" `pOrElse` pLit "&" `pOrElse` pLit "|")
  (pInfixExpr `pOrElse` pExpr)
  
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
  ((pLit "let" `pOrElse` pLit "letrec") `pApply` (== "letrec"))
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
