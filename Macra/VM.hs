module Macra.VM (Value(..), Identifier(..), Inst(..), {--vm--}) where

import qualified Data.Unique as U
import qualified Data.Map as M
import qualified Data.List as L
import qualified Control.Monad.State as S

type Identity = U.Unique
data Value = Double  Double
           | Char    Char
           | List    [Value]
           | Closure Identifier Inst Env Identity
           | Thunk   Inst            Env
           deriving (Eq, Ord)

instance Show Value where
  show (Char c) = [c]
  show (Double i) = show i
  show (List xs) = concat ["(", concat (L.intersperse " " (map show xs)), ")"]
  show (Closure var body _ _) = concat [show "Close: ", show var, show body]
  show (Thunk body e) = show body

type Identifier = String
data Inst = ConstExpr  Value           -- ldc
          | ConsInst
          | CarInst
          | CdrInst
          | CloseInst  Identifier Inst -- ldf
          | FreezeInst Inst
          | ApplyInst                  -- ap
          | ThawInst
          | ReferInst  Identifier      -- ld
          | ReturnInst                 -- ret
          | TestInst   Inst       Inst -- sel
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
  show (VM s e c d ge) = concat ["S: ", show s, "\n",
                                 "E: ", show e, "\n",
                                 "C: ", show c, "\n",
                                 "D: ", show d, "\n",
                                 "G: ", show ge, "\n"]

type Stack = [Value]
type Env = M.Map Identifier Value
type Code = [Inst]
type Dump = [([Value], Env, [Inst])]

