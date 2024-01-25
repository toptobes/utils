{-# LANGUAGE TemplateHaskell #-}

module UtAction where

import Control.Monad.Free
import Control.Monad.Free.TH
import System.Process
import Utils
import UtConfig

type UtActionF = Free UtAction

data UtAction next
  = RunSysCmd Text next
  | PrintText Text next
  | WithTemplate Text (Text -> next)
  | WithAbsTPath Text (Text -> next)
  | WithCurrYear (Integer -> next)
  | WithCfgPath  (Text -> next)
  | WithCfg  (UTConfig -> next)
  | SaveCfg   UTConfig next
  deriving (Functor)

$(makeFree ''UtAction)

runActions :: Free UtAction a -> IO a
runActions = iterM go where 
  go (RunSysCmd (toString -> cmd) next) = system cmd >> next
  go (PrintText text next) = putTextLn text >> next
  go (WithTemplate path next) = readTemplate path >>= next
  go (WithAbsTPath path next) = templatePath path >>= next 
  go (WithCurrYear next) = year >>= next
  go (WithCfgPath next) = configPath >>= next
  go (WithCfg next) = readConfig >>= next
  go (SaveCfg cfg next) = saveConfig cfg >> next
  