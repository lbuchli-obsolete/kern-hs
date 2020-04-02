module TemplateInst where

import Language
import Util

type TiState = (TiStack, TiDump, TiHeap, TiGlobals, TiStats)
type TiStack = [Addr]
data TiDump = DummyTiDump deriving Show
type TiHeap = Heap Node
type TiGlobals = Map Name Addr
type TiStats = Integer

data Node = NAp Addr Addr
          | NSupercomb Name [Name] KernExpr
          | NNum Integer
  deriving Show


tiStatInitial :: TiStats
tiStatIncSteps :: TiStats -> TiStats
tiStatGetSteps :: TiStats -> Integer

tiStatInitial = 0
tiStatIncSteps s = s + 1
tiStatGetSteps s = s

initialTiDump :: TiDump
initialTiDump = DummyTiDump

applyToStats :: (TiStats -> TiStats) -> TiState -> TiState
applyToStats stats_fun (stack, dump, heap, sc_defs, stats)
  = (stack, dump, heap, sc_defs, stats_fun stats)

compile :: KernProgram -> TiState
compile program =
  (initial_stack, initialTiDump, initial_heap, globals, tiStatInitial)
  where
    sc_defs = program ++ preludeDefs
  
    (initial_heap, globals) = buildInitialHeap sc_defs
    initial_stack = [aLookup globals "main"]

buildInitialHeap :: [KernScDefn] -> (TiHeap, TiGlobals)
buildInitialHeap = mapAccuml allocateSc hInitial

allocateSc :: TiHeap -> KernScDefn -> (TiHeap, (Name, Addr))
allocateSc heap (name, args, body) = (heap', (name, addr))
  where
    (heap', addr) = hAlloc heap (NSupercomb name args body)
  
eval :: TiState -> [TiState]
eval state = state : rest_states
  where
    rest_states | tiFinal state = []
                | otherwise = eval next_state
    next_state = doAdmin (step state)

doAdmin :: TiState -> TiState
doAdmin = applyToStats tiStatIncSteps

tiFinal :: TiState -> Bool
tiFinal ([sole_addr], _, heap, _, _) = isDataNode (hLookup heap sole_addr)
tiFinal ([], _, _, _, _)             = error "Empty stack!"
tiFinal _ = False -- Stack contains more than one item

isDataNode :: Node -> Bool
isDataNode (NNum _) = True
isDataNode _        = False

step :: TiState -> TiState
step state = dispatch (hLookup heap (head stack))
  where
    (stack, _, heap, _, _) = state
    dispatch (NNum n)                  = numStep state n
    dispatch (NAp a1 a2)               = apStep  state a1 a2
    dispatch (NSupercomb sc args body) = scStep  state sc args body

numStep :: TiState -> Integer -> TiState
numStep _ _ = error "Number applied as a function!"

apStep :: TiState -> Addr -> Addr -> TiState
apStep (stack, dump, heap, globals, stats) a1 _ = (a1 : stack, dump, heap, globals, stats)

scStep :: TiState -> Name -> [Name] -> KernExpr -> TiState
scStep (stack, dump, heap, globals, stats) _ arg_names body
  = (new_stack, dump, new_heap, globals, stats)
  where
    new_stack = result_addr : drop (length arg_names+1) stack
    (new_heap, result_addr) = instantiate body heap env
    env = arg_bindings ++ globals
    arg_bindings = zip arg_names (getArgs heap stack)

getArgs :: TiHeap -> TiStack -> [Addr]
getArgs heap (_:stack) = map get_arg stack
  where get_arg addr = arg where (NAp _ arg) = hLookup heap addr
getArgs _ [] = error "Trying to get args where there is no stack :("

instantiate ::
  KernExpr ->      -- Body of supercombinator
  TiHeap ->        -- Heap before instantiation
  Map Name Addr -> -- Association of names to addresses
  (TiHeap, Addr)   -- Heap after instantiation and address of root instance
  
instantiate (ENum n) heap _ = hAlloc heap (NNum n)
instantiate (EAp e1 e2) heap env = hAlloc heap'' (NAp a1 a2)
  where
    (heap', a1)  = instantiate e1 heap env
    (heap'', a2) = instantiate e2 heap' env
instantiate (EVar v) heap env = (heap, aLookup env v)
instantiate (EConst tag arity) heap env = instantiateConst tag arity heap env
instantiate (ELet isrec defs body) heap env = instantiateLet isrec defs body heap env
instantiate (ECase _ _) _ _ = error "Can't instantiate case expressions :("
instantiate (ELam _ _) _ _ = error "What lambda?"

instantiateConst :: Integer -> Integer -> TiHeap -> Map Name Addr -> (TiHeap, Addr)
instantiateConst _ _ _ _ = error "Can't instantiate constructors yet :("

instantiateLet :: Bool -> [(Name, KernExpr)] -> KernExpr -> TiHeap -> Map Name Addr -> (TiHeap, Addr)
instantiateLet _ _ _ _ _ = error "Can't instantiate let expressions yet!"

----------------------------------------------------
--                 Pretty Printing                --
----------------------------------------------------

showResults :: [TiState] -> String
showResults states = iDisplay $ iConcat
                     [ iLayn (map showState states), showStats (last states) ]

showState :: TiState -> Iseq
showState (stack, _, heap, _, _) = iConcat [ showStack heap stack, iNewline ]

showStack :: TiHeap -> TiStack -> Iseq
showStack heap stack = iConcat [ iStr "Stk [",
                                 iIndent (iInterleave iNewline (map show_stack_item stack)),
                                 iStr " ]"
                               ]
  where
    show_stack_item addr = iConcat [
      showFWAddr addr,
      iStr ": ",
      showStkNode heap (hLookup heap addr)
                                   ]

showStkNode :: TiHeap -> Node -> Iseq
showStkNode heap (NAp _ arg_addr) = iConcat [ iStr "NAp ", showFWAddr arg_addr, iStr " (",
                                              showNode (hLookup heap arg_addr), iStr ")"
                                            ]
showStkNode _ node = showNode node

showNode :: Node -> Iseq
showNode (NAp a1 a2) = iConcat [
  iStr "NAp ", showAddr a1,
  iStr " ", showAddr a2
                               ]
showNode (NSupercomb name _ _) = iStr ("NSupercomb " ++ name)
showNode (NNum n) = iStr "NNum " `iAppend` iNum n

showAddr :: Addr -> Iseq
showAddr addr = iStr $ show addr

showFWAddr :: Addr -> Iseq -- Show address in field of width 4
showFWAddr addr = iStr $ space (4 - length str) ++ str
  where
    str = show addr

showStats :: TiState -> Iseq
showStats (_, _, _, _, stats) = iConcat [ iNewline, iNewline, iStr "Total number of steps = ",
                                          iNum (tiStatGetSteps stats)
                                        ]
