import Language.UHC.JS.ECMA.String
import Language.UHC.JS.Assorted
import Language.UHC.JS.Primitives
import Language.UHC.JS.JQuery.JQuery
import UHC.Ptr


foreign import jscript "getJSFun(%1)"
  getJSFun :: Int -> IO (FunPtr (Int -> Int))

foreign import jscript "dynamic"
  mkDyn :: FunPtr (Int -> Int) -> (Int -> Int)

main :: IO ()
main = do
  putStrLn "import_dynamic"
  jfn <- getJSFun 2
  print $ (mkDyn jfn) 3

