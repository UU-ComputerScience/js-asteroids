{-# LANGUAGE CPP, MultiParamTypeClasses, FlexibleInstances, DeriveDataTypeable, StandaloneDeriving, FlexibleContexts, ScopedTypeVariables, NoMonomorphismRestriction #-}
{-# OPTIONS -pgmP cpp #-}
module Examples.ListSimpleSelf where
import LightOO
import Data.Dynamic
import Data.IORef

#ifdef __UHC__
#include "../LightOOUHC.h"
#include "Typeable.h"
#else
#include "../LightOO.h"
#endif

data IList a = IList
   { _listIsEmpty :: IO Bool
   , _listGetHead :: IO String
   , _listGetTail :: IO List
   , _listSetHead :: String -> IO ()
   , _listInsHead :: String -> IO List
   , _listTail :: Record a
   }

DefineClass(List,IList,listTail,,1)

#ifdef __UHC__
INSTANCE_TYPEABLE1(IList,listTc,"List")
#endif

listIsEmpty = _listIsEmpty  
listGetHead = _listGetHead
listGetTail = _listGetTail
listSetHead = _listSetHead
listInsHead = _listInsHead

nilList = clazz $ \tail self -> do
   return IList {
      _listIsEmpty = return True
     ,_listGetHead = fail "no head"
     ,_listGetTail = self # listIsEmpty >> fail "no tail"
     ,_listSetHead = const (fail "no head")
     ,_listInsHead = reusableInsHead self
     ,_listTail = tail
   }

reusableInsHead :: (Sub (IList a) List) => IList a -> String -> IO List
reusableInsHead list head = do
   newCons <- new $ consList head (upcast list :: List)
   return newCons
{-
consList :: (ListClass a :<: List) =>
     String -> List -> IO (ListClass a -> Record b) -> ListClass a -> IO (IList b)
-}
consList head t = clazz $ \tail self -> do 
   hRef <- newIORef head
   return IList {
      _listIsEmpty = return False
     ,_listGetHead = readIORef hRef
     ,_listGetTail = return t
     ,_listSetHead = writeIORef hRef
     ,_listInsHead = reusableInsHead self
     ,_listTail = tail
   } 

data IReverseList a = IReverseList {
   _reverseListEcho :: IO ()
   ,_reverseListTail :: Record a
}
DefineSubClass(ReverseList,List,IReverseList,reverseListTail,,,,1,)

#ifdef __UHC__
INSTANCE_TYPEABLE1(IReverseList,reverseListTc,"ReverseList")
#endif

{-
nilReverseList :: (ReverseList_ a :<: List) =>
     IO (ReverseList_ a -> Record b) -> ReverseList_ a -> IO (ReverseList_ b) 
-}
nilReverseList = 
   (wrapper `extends` nilList) noOverride set_List_Tail
   where
   wrapper tail super self =
      return IReverseList {
          _reverseListEcho = putStrLn "I'm a reverse cons"
         ,_reverseListTail = tail
      }

reverseListEcho = _reverseListEcho . unRecord . _listTail

{-
consReverseList :: (ReverseList_ a :<: List) =>
     String -> ReverseList -> IO (ReverseList_ a -> Record b) -> ReverseList_ a -> IO (ReverseList_ b) -}
consReverseList head tail = 
   (wrapper `extends` consList (reverse head) (upcast tail :: List)) noOverride set_List_Tail
   where
   wrapper tail super self = 
      return IReverseList {
          _reverseListEcho = putStrLn "I'm a reverse cons"
         ,_reverseListTail = tail
      }

hiList' = do
   l <- new nilReverseList
   l <- new $ consReverseList "dlrow" l 
   l <- new $ consReverseList "olleh" l
   return l

hiList = do
   l <- new nilList
   l <- new $ consList "2" l
   l <- new $ consList "4" l 
   return l
   
sayHi ls = do
   l <- ls
   printList (upcast l :: List)

printList aList = do
   empty <- aList # listIsEmpty
   if empty
      then putStrLn ""
      else do 
            head <- aList # listGetHead
            putStr $ head
            tail <- aList # listGetTail
            putStr " "
            printList tail

