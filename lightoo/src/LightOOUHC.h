#define DefineClass(X,XC,XTAIL,AP,NP) \
type X ## _ AP t = XC AP t ; \
type X AP = X ## _ AP () ; \
\
instance ModTail (XC AP) where { \
   getTail = _ ## XTAIL ; \
   setTail o v = o { _ ## XTAIL = v } } ; \
\
get_ ## X ## _Tail :: X ## _ AP t -> Record t ; \
get_ ## X ## _Tail = getTail ; \
set_ ## X ## _Tail :: X ## _ AP t -> Record tt -> X ## _ AP tt ; \
set_ ## X ## _Tail o v = setTail o v ; \
modify_ ## X ## _Tail = mkMod set_ ## X ## _Tail get_ ## X ## _Tail ;

#define DefineSubClass(X,Y,XC,XTAIL,AP,YP,XP,NP,CONSTR) \
type X ## _ AP t = Y ## _ YP (XC XP t) ; \
type X AP = X ## _ AP () ; \
\
instance (CONSTR) => Narrow (X AP) (Y YP) where { \
   narrow = modify_ ## Y ## _Tail hideRecord } ; \
\
instance (CONSTR) => Widen (Y YP) (X AP) where { \
   widen o = genericWiden o get_ ## Y ## _Tail set_ ## Y ## _Tail } ; \
\
instance ModTail (XC XP) where { \
   getTail = _ ## XTAIL ; \
   setTail o v = o { _ ## XTAIL = v } } ; \
\
get_ ## X ## _Tail :: X ## _ AP t -> Record t ; \
get_ ## X ## _Tail = getTail . unRecord . get_ ## Y ## _Tail ; \
set_ ## X ## _Tail :: X ## _ AP t -> Record tt -> X ## _ AP tt ; \
set_ ## X ## _Tail o v = modify_ ## Y ## _Tail (\o -> record $ setTail (unRecord o) v) o ; \
modify_ ## X ## _Tail = mkMod set_ ## X ## _Tail get_ ## X ## _Tail ;
