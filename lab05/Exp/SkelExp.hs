-- File generated by the BNF Converter (bnfc 2.9.4).

-- Templates for pattern matching on abstract syntax

{-# OPTIONS_GHC -fno-warn-unused-matches #-}

module Exp.SkelExp where

import Prelude (($), Either(..), String, (++), Show, show)
import qualified Exp.AbsExp

type Err = Either String
type Result = Err String

failure :: Show a => a -> Result
failure x = Left $ "Undefined case: " ++ show x

transIdent :: Exp.AbsExp.Ident -> Result
transIdent x = case x of
  Exp.AbsExp.Ident string -> failure x

transExp :: Exp.AbsExp.Exp -> Result
transExp x = case x of
  Exp.AbsExp.ELet ident exp1 exp2 -> failure x
  Exp.AbsExp.EAdd exp1 exp2 -> failure x
  Exp.AbsExp.ESub exp1 exp2 -> failure x
  Exp.AbsExp.EMul exp1 exp2 -> failure x
  Exp.AbsExp.EInt integer -> failure x
  Exp.AbsExp.EVar ident -> failure x
