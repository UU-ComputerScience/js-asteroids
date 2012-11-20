{-
// Introduced in DOM Level 2:
interface CSSStyleDeclaration {
           attribute DOMString        cssText;
                                        // raises(DOMException) on setting

  DOMString          getPropertyValue(in DOMString propertyName);
  CSSValue           getPropertyCSSValue(in DOMString propertyName);
  DOMString          removeProperty(in DOMString propertyName)
                                        raises(DOMException);
  DOMString          getPropertyPriority(in DOMString propertyName);
  void               setProperty(in DOMString propertyName, 
                                 in DOMString value, 
                                 in DOMString priority)
                                        raises(DOMException);
  readonly attribute unsigned long    length;
  DOMString          item(in unsigned long index);
  readonly attribute CSSRule          parentRule;
};
-}
module Language.UHC.JS.HTML5.CSSStyleDeclaration where

import Language.UHC.JS.HTML5.Types
import Language.UHC.JS.Types

foreign import js "%1.cssText"
   cssText :: CSSStyleDeclaration -> IO JSString

foreign import js "%1.getPropertyValue(%*)"
   getPropertyValue :: CSSStyleDeclaration -> JSString -> IO JSString

foreign import js "%1.getPropertyCSSValue(%*)"
   getPropertyCSSValue :: CSSStyleDeclaration -> IO CSSValue

foreign import js "%1.getPropertyPriority(%*)"
   getPropertyPriority :: CSSStyleDeclaration -> IO JSString

foreign import js "%1.setProperty(%*)"
   setProperty :: CSSStyleDeclaration -> JSString -> a -> IO ()

foreign import js "%1.length"
   length :: CSSStyleDeclaration -> IO Int

foreign import js "%1.item(%*)"
   item :: CSSStyleDeclaration -> IO JSString

foreign import js "%1.parentRule"
   parentRule :: CSSStyleDeclaration -> IO CSSRule

{-
// Introduced in DOM Level 2:
interface CSSValue {

  // UnitTypes
  const unsigned short      CSS_INHERIT                    = 0;
  const unsigned short      CSS_PRIMITIVE_VALUE            = 1;
  const unsigned short      CSS_VALUE_LIST                 = 2;
  const unsigned short      CSS_CUSTOM                     = 3;

           attribute DOMString        cssText;
                                        // raises(DOMException) on setting

  readonly attribute unsigned short   cssValueType;
};
-}