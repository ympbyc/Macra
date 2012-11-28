module Macra.VM (Value(..), Identifier(..), Inst(..), {--vm--}) where

import qualified Data.Unique as U
import qualified Data.Map as M
import qualified Data.List as L
import qualified Control.Monad.State as S

type Identity = U.Unique
data Value = Double  Double
           | Bool    Bool
           | Char    Char
           | List    [Value]
           | Closure Identifier Code Env --Identity
           | Thunk   Code            Env
           deriving (Eq, Ord)

instance Show Value where
  show (Char c) = [c]
  show (Double i) = show i
  show (List xs) = concat ["(", concat (L.intersperse " " (map show xs)), ")"]
  show (Closure var body _) = concat [show "Close: ", show var, show body]
  show (Thunk body e) = show body

type Identifier = String
data Inst = ConstExpr  Value           -- ldc
          | ConsInst
          | CarInst
          | CdrInst
          | CloseInst  Identifier Code -- ldf
          | FreezeInst Code
          | ApplyInst                  -- ap
          | ThawInst
          | ReferInst  Identifier      -- ld
          | ReturnInst                 -- ret
          | TestInst   Code       Code -- sel
          | JoinInst
          | RestoreInst
          | DefineInst Identifier
          | HaltInst
          | PrintInst          
          | EqualInst
          deriving (Show, Eq, Ord)

data VM = VM {
     vmStack :: Stack
   , vmEnv :: Env
   , vmCode :: Code
   , vmDump :: Dump
   , vmGlobalEnv :: Env
     }

instance Show VM where
  show (VM s e c d ge) = concat [ "S: ", show s, "\n"
                                , "E: ", show e, "\n"
                                , "C: ", show c, "\n"
                                , "D: ", show d, "\n"
                                , "G: ", show ge, "\n"
                                ]

type Stack = [Value]
type Env   = M.Map Identifier Value
type Code  = [Inst]
type Dump  = [(Stack, Env, Code)]
type VMCommand = S.StateT VM IO ()


lookupVal :: Identifier -> Env -> Maybe Value
lookupVal idtf env = M.lookup idtf env

nil :: Value
nil = List []

true :: Value
true = Double 0

false :: Value
false = nil

vm :: Inst -> IO ()
vm inst = do
  S.evalStateT vm' VM {
    vmStack = []
  , vmEnv   = initialEnv
  , vmCode  = [inst]
  , vmDump  = []
  , vmGlobalEnv  = M.empty
  }
  where initialEnv = M.fromList [ ("nil", nil) ]

vm' :: VMCommand
vm' = S.get >>= vm''


-- VM --
vm'' :: VM -> VMCommand

vm'' (VM _ _ (HaltInst:_) _ _) = return ()

-- cons constant value onto the stack
vm'' vmState@(VM s _ ((ConstExpr val):nxt) _ _) = do
  S.put vmState {
    vmStack = val:s
  , vmCode  = nxt  
  }
  vm'

-- print the content on the stack
vm'' vmState@(VM s e (PrintInst:nxt) d g) = do
  S.liftIO $ print s
  S.put vmState {
    vmCode  = nxt
  }
  vm'

-- refer the value associated with idtf in env
vm'' vmState@(VM s e ((ReferInst idtf):nxt) _ g) = do
  case lookupVal idtf e of -- refer local
    Just v -> do
      S.put vmState {
        vmStack = v:s
      , vmCode  = nxt
      }
      vm'
    Nothing -> do
      case lookupVal idtf g of  -- refer global
        Just v -> do
          S.put vmState {
            vmStack = v:s
          , vmCode  = nxt
          }
          vm'
        Nothing -> do
          S.liftIO $ do
            putStr $ concat ["unbound variable: `", idtf, "'"]
            return ()

-- make a closure and cons it onto the stack
vm'' vmState@(VM s e ((CloseInst param code):nxt) _ _) = do
  S.put vmState {
    vmStack = (Closure param code e):s
  , vmCode  = nxt
  }
  vm'

-- create a call frame and evaluate the closure applying the value at stack top
vm'' vmState@(VM ((Closure param code env):arg:rest) e (ApplyInst:nxt) d _) = do
  S.put vmState {
    vmStack = rest
  , vmEnv   = M.insert param arg env
  , vmCode  = code
  , vmDump  = (rest, e, nxt):d
  }
  vm'

-- restore the dump, cons the stack top onto the now-current stack
vm'' vmState@(VM (retVal:_) _ (RestoreInst:_) ((rS, rE, rC):dRest) _) = do
  S.put vmState {
    vmStack = retVal:rS
  , vmEnv   = rE
  , vmCode  = rC
  , vmDump  = dRest
  }
  vm'

-- if
vm'' vmState@(VM (bool:sRest) _ ((TestInst tClause fClause):nxt) d g) = do
  S.put vmState {
    vmStack = sRest
  , vmCode  = case bool of
                false -> fClause
                _ -> tClause
  , vmDump  = ([], M.empty, nxt):d
  }
  vm'

-- rejoin after the execution of TestInst
vm'' vmState@(VM _ _ (JoinInst:_) ((_, _, nxt):dRest) _) = do
  S.put vmState {
    vmCode = nxt
  , vmDump = dRest
  }
  vm'

-- bind the value at the top of the stack to identifier globally
vm'' vmState@(VM (val:sRest) _ ((DefineInst idtf):nxt) _ g) = do
  S.put vmState {
    vmStack = sRest
  , vmCode  = nxt
  , vmGlobalEnv = M.insert idtg val g
  }
  vm'

-- delay the evaluation of the code until thawed
vm'' vmState@(VM s e ((FreezeInst fCode):nxt) _ _) = do
  S.put vmState {
    vmStack = (Thunk fCode e):s
  }
  vm'

-- evaluate the code inside the thunk
vm'' vmState@(VM (Thunk tCode tEnv):sRest e (ThawInst:nxt) d _) = do
  S.put vmState {
    vmStack = []
  , vmEnv   = tEnv
  , vmCode  = tCode
  , vmDump  = (sRest, e, nxt):d
  }
  vm'

vm'' vmState = do
  S.liftIO $ do
    print "** VM BUG **: "
    print vmState
