{-# LANGUAGE TemplateHaskell #-}

module UtAction where

import Control.Monad.Free
import Control.Monad.Free.TH
import System.Process
import Utils

type UtActionF = Free UtAction

data UtAction next
  = RunSysCmd Text next
  | PrintText Text next
  | WTemplate Text (Text -> next)
  | WAbsTPath Text (Text -> next)
  | WCurrYear (Integer -> next)
  deriving (Functor)

$(makeFree ''UtAction)

runActions :: Free UtAction a -> IO a
runActions = iterM go where 
  go (RunSysCmd (toString -> cmd) next) = system cmd >> next
  go (PrintText text next) = putTextLn text >> next
  go (WTemplate path next) = readTemplate path >>= next
  go (WAbsTPath path next) = templatePath path >>= next 
  go (WCurrYear next) = year >>= next
