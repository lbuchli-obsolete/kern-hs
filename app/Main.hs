module Main where

-- import Lib

import Language
import Lexer
import Parser

main :: IO ()
main = do
  -- putStrLn (pprintProg preludeDefs)
  -- print (klex 0 "add a b = \n a + b if \n a == b -- This is a comment \n this is not")
  -- print (klex 0 "a a a")
  -- print (pBoolArray (klex 0 "[True, False, True, True]"))
  -- print (pZeroOrMore (pLit "A") (klex 0 "A A A"))
  -- print (pThen (++) (pLit "Help") (pLit "me") (klex 0 "Help me"))
  putStrLn (pprintProg (kParse (klex 0 (unlines [
                                          "f = 3;",
                                          "g x y = let z = x in z;",
                                          "h x = case (let y = x in y) of",
                                          "    <1> => 2;",
                                          "    <2> => 5;"
                                               ]))))
