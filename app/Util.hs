module Util where

first :: (a, b, c) -> a
first (x, _, _) = x

second :: (a, b, c) -> b
second (_, x, _) = x

third :: (a, b, c) -> c
third (_, _, x) = x

mapLeft :: (a0 -> a) -> Either a0 b -> Either a b
mapLeft f (Left a0) = Left (f a0)
mapLeft _ (Right b) = Right b

mapAccuml ::
  (a -> b -> (a, c)) -> -- Function of accumulator and element input list, returning new
                        --   accumulator and element of result list
  a ->                  -- Initial accumulator
  [b] ->                -- Input list
  (a, [c])              -- Final accumulator and result list

mapAccuml _ acc [] = (acc, [])
mapAccuml f acc (x:xs) = (acc2, x':xs')
  where
    (acc1, x') = f acc x
    (acc2, xs') = mapAccuml f acc1 xs

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
                  "" -> []
                  s' -> w : wordsWhen p s''
                    where (w, s'') = break p s'

space :: Int -> String
space 0 = ""
space n = " " ++ space (n - 1)

---------------------------------------------------------
--                         Heap                        --
---------------------------------------------------------

type Heap a = (Int, [Int], Map Int a)
type Addr   = Int
  
hInitial   :: Heap a
hAlloc     :: Heap a -> a -> (Heap a, Addr)
hUpdate    :: Heap a -> Addr -> a -> Heap a
hFree      :: Heap a -> Addr -> Heap a
hLookup    :: Heap a -> Addr -> a
hAddresses :: Heap a -> [Addr]
hSize      :: Heap a -> Int
hNull      :: Addr
hIsNull    :: Addr -> Bool

hInitial = (0, [1..], [])
hAlloc (size, next:free, cts) node = ((size+1, free, (next, node):cts), next)
hAlloc _ _ = error "None of the infinite addresses are left (╯°□°)╯ ┻━┻"
hUpdate (size, free, cts) addr node = (size, free, (addr, node) : filter (\x -> fst x /= addr) cts)
hFree (size, free, cts) addr = (size-1, addr:free, filter (\x -> fst x /= addr) cts)
hLookup (_, _, cts) = aLookup cts
hAddresses (_, _, cts) = [addr | (addr, _) <- cts]
hSize (size, _, _) = size
hNull = 0
hIsNull a = a == 0

-------------------------------------------------------------------
--                              Map                              --
-------------------------------------------------------------------

type Map a b = [(a, b)]

aLookup :: Eq a => Show a => Map a b -> a -> b
aLookup ((k, v):bs) k' | k == k' = v
                       | k /= k' = aLookup bs k'
aLookup _ k'                     = error ("Can't find #" ++ show k' ++ " in map.")

aDomain :: Map a b -> [a]
aDomain aMap = [key | (key, _) <- aMap]

aRange :: Map a b -> [b]
aRange aMap = [val | (_, val) <- aMap]

aEmpty :: Map a b
aEmpty = []

----------------------------------------------------------------------
--                               Iseq                               --
----------------------------------------------------------------------

data Iseq = INil
          | IStr String
          | IAppend Iseq Iseq
          | IIndent Iseq
          | INewline

iNil :: Iseq                          -- The empty iseq
iStr :: String -> Iseq                -- Turn a string into an iseq
iAppend :: Iseq -> Iseq -> Iseq       -- Append two iseqs
iNewline :: Iseq                      -- New line with indentation
iIndent :: Iseq -> Iseq               -- Indent an iseq
iDisplay :: Iseq -> String            -- Turn an iseq into a string
iConcat :: [Iseq] -> Iseq             -- Concatenate multiple iseqs
iInterleave :: Iseq -> [Iseq] -> Iseq -- Concatenate with delimiter

iNil    = INil
iAppend = IAppend
iStr str = iInterleave INewline (map IStr (wordsWhen (== '\n') str))

iConcat (seq1 : seq2 : seqs) = iConcat (iAppend seq1 seq2 : seqs)
iConcat (seq1 : _)           = seq1
iConcat _                    = INil

iInterleave del (seq1 : _ : seqs) | length seqs > 1 = iConcat (iAppend seq1 del : seqs)
iInterleave _ (seq1 : _)                            = seq1
iInterleave _ _                                     = INil

iIndent seq' = seq'
iNewline    = IStr "\n"

iNum :: Integer -> Iseq
iNum n = iStr (show n)

iFWNum :: Int -> Integer -> Iseq
iFWNum width n = iStr $ space (width - length digits) ++ digits
  where
    digits = show n

iLayn :: [Iseq] -> Iseq
iLayn seqs = iConcat $ zipWith (curry lay_item) [1..] seqs
  where
    lay_item (n, seq') = iConcat [iFWNum 4 n, iStr ") ", iIndent seq', iNewline]
    

flatten :: Int ->           -- Current column; 0 for first column
           [(Iseq, Int)] -> -- Work list
           String           -- Result
           
flatten _ [] = ""
flatten col ((INil, _) : seqs)              = flatten col seqs
flatten col ((IStr s, _) : seqs)            = s ++ flatten col seqs
flatten col ((IAppend seq1 seq2, _) : seqs) = flatten col ((seq1, col) : (seq2, col) : seqs)
flatten col ((IIndent seq', _) : seqs)      = flatten col ((seq', col) : seqs)
flatten _   ((INewline, indent) : seqs)     = '\n' : space indent ++ flatten indent seqs
  
iDisplay seq' = flatten 0 [(seq', 0)]
