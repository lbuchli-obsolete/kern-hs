module Lexer where

import Data.Char

type Token = (Int, TokenType, String)

data TokenType = KNumber | KTwoCharOperator | KVariable | KOther
  deriving (Eq, Show)

klex :: Int -> String -> [Token]
klex _ [] = []
klex line (c:cs) | isSpace c = klex new_line cs                                        -- Whitespace
                 where
                   new_line = if c == '\n' then line + 1 else line
                     
klex line cs | take 2 cs == "--" = klex (line + 1) (dropWhile (/= '\n') cs)            -- Comments 
klex line (c:cs) | isDigit c = (line, KNumber, num_token) : klex line rest_cs          -- Numbers
            where
              num_token = c : takeWhile isDigit cs
              rest_cs = dropWhile isDigit cs
klex line cs | take 2 cs `elem` twoCharOps =                                           -- Two char operators
               (line, KTwoCharOperator, take 2 cs) : klex line (drop 2 cs)
klex line (c:cs) | isAlpha c = (line, KVariable, var_token) : klex line rest_cs        -- Variables
            where
              var_token = c : takeWhile isIdChar cs
              rest_cs = dropWhile isIdChar cs
klex line (c:cs) = (line, KOther, [c]) : klex line cs                                  -- Other characters


isIdChar :: Char -> Bool
isIdChar c = isAlpha c || isDigit c || (c == '_')

twoCharOps :: [String]
twoCharOps = ["==", "!=", ">=", "<=", "=>", "->"]
