module Graphics.UI.WXCore.EvtHandler (
   module Graphics.UI.WXCore.EvtHandlerClass
  ,evthandler
) where

import Graphics.UI.WXCore.EventClass
import Graphics.UI.WXCore.EvtHandlerClass
import Graphics.UI.WXCore.Types
import LightOO
import Data.IORef

a `onlyIf` b = do
   a' <- a
   if a' then b else return False

lookupEntryByEventType t = filter $ (t ==) . dynamicEventTableEntryEventType 

evthandler =
   (evthandler' `extends` object) noOverride set_Object_Tail
   where
   evthandler' tail super self = do
      handlers <- newIORef [] 
      return EvtHandlerClass {
           _evtHandlerAddPendingEvent = error "_evtHandlerAddPendingEvent not implemented"
         , _evtHandlerBind = \evtty f id lastId -> do
            let entry = DynamicEventTableEntry {
                dynamicEventTableEntryEventType = evtty
               ,dynamicEventTableEntryId = id
               ,dynamicEventTableEntryLastId = lastId
               ,dynamicEventTableEntryFunc = f
               ,dynamicEventTableEntryUserData = error "dynamicEventTableEntryUserData not implemented"
            } 
            modifyIORef handlers (entry :)

         , _evtGetHandler = \evtty id lid ->
            let findHandler entry = 
                  let eid  = dynamicEventTableEntryId entry
                      elid = dynamicEventTableEntryLastId entry
                      eevtty = dynamicEventTableEntryEventType entry 
                  in eid == id && (elid == lid || lid == wxID_ANY) && (eevtty == evtty)

                maybeHead []        = Nothing
                maybeHead (entry:_) = Just (dynamicEventTableEntryFunc entry)

            in readIORef handlers >>= return . maybeHead . filter findHandler

         , _evtHandlerUnBind = \evtty id lid ->
            let doUnbind entry = 
                  let eid  = dynamicEventTableEntryId entry
                      elid = dynamicEventTableEntryLastId entry
                      eevtty = dynamicEventTableEntryEventType entry 
                  in not $ (eid == id && (elid == lid || lid == wxID_ANY) && (eevtty == evtty))
            in modifyIORef handlers (filter doUnbind)

         , _evtHandlerDeletePendingEvents = error "_evtHandlerDeletePendingEvents not implemented"
         , _evtHandlerGetEvtHandlerEnabled = error "_evtHandlerGetEvtHandlerEnabled not implemented"
         , _evtHandlerGetNextHandler = error "_evtHandlerGetNextHandler not implemented"
         , _evtHandlerGetPreviousHandler = error "_evtHandlerGetPreviousHandler not implemented"
         , _evtHandlerIsUnlinked = error "_evtHandlerIsUnlinked not implemented"
         , _evtHandlerProcessEvent = \evt ->
            let before = return True
                processLocally = self # evtHandlerProcessEventLocally $ evt
                after = return True
            in before `onlyIf` processLocally `onlyIf` after 
                     
         , _evtHandlerProcessEventLocally = \evt -> 
            let processEventIfMatchesId entry = do
                  let id  = dynamicEventTableEntryId entry
                      lid = dynamicEventTableEntryLastId entry
                  eid <- evt # eventGetId
                  if ((id == wxID_ANY) || (lid == wxID_ANY && id == eid) || (lid /= wxID_ANY && (eid >= id && eid <= lid)))
                     then do evt # eventSkip $ False
                             -- set callback data
                             (dynamicEventTableEntryFunc entry) evt
                             return True
                     else return True

            in do entries <- readIORef handlers
                  eventType <- evt # eventGetType
                  let matchingHandlers = lookupEntryByEventType eventType entries
                  mapM_ processEventIfMatchesId matchingHandlers
                  return True

         , _evtHandlerProcessPendingEvents = error "_evtHandlerProcessPendingEvents not implemented"
         , _evtHandlerQueueEvent = error "_evtHandlerQueueEvent not implemented"      
         , _evtHandlerSetEvtHandlerEnabled = error "_evtHandlerSetEvtHandlerEnabled not implemented"
         , _evtHandlerSetNextHandler = error "_evtHandlerSetNextHandler not implemented"
         , _evtHandlerSetPreviousHandler = error "_evtHandlerSetPreviousHandler not implemented"
         , _evtHandlerTryAfter = error "_evtHandlerTryAfter not implemented"
         , _evtHandlerTryBefore = error "_evtHandlerTryBefore not implemented"
         , _evtHandlerTryThis = error "_evtHandlerTryThis not implemented"
         , _evtHandlerUnlink = error "_evtHandlerUnlink not implemented"
         , _evtHandlerTail = tail
      }