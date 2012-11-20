module Language.UHC.JS.JQuery.Deferred where

import Language.UHC.JS.Prelude
import Language.UHC.JS.Types

boundExecution :: IO a -> IO a -> Int -> (a -> IO b)  -> (a -> IO b) -> IO ()
boundExecution calc fallback timeout onCalc onFallback = do
  calc'       <- wrapFunc calc
  fallback'   <- wrapFunc fallback
  onCalc'     <- wrapFunc1 onCalc
  onFallback'  <- wrapFunc1 onFallback
  _boundExecution calc' fallback' timeout onCalc' onFallback'

foreign import js "boundExecution(%*)"
  _boundExecution :: JSFunction_ (IO a) -> JSFunction_ (IO a) -> Int -> JSFunction_ (a -> IO b) -> JSFunction_ (a -> IO b) -> IO ()