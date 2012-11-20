-- | Binding for the jquery-ajaxq library by Oleg Podolsky.
--   It can be found at: http://code.google.com/p/jquery-ajaxq/
module Language.UHC.JS.JQuery.AjaxQueue (ajaxQ) where

import Language.UHC.JS.Types
import Language.UHC.JS.Marshal
import Language.UHC.JS.JQuery.Ajax

-- | Partial application of the backend for use with the AjaxQueue library  
ajaxQ  :: String -> AjaxOptions a -> v -> AjaxCallback r -> AjaxCallback r -> IO ()
ajaxQ queuename = ajaxBackend (_ajaxQ $ toJS queuename)
  
foreign import js "$.ajaxq(%*)"
  _ajaxQ :: JSString -> JSAny a -> IO ()