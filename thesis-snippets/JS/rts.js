primIsFunction = function(a) {
   return PrimMkBool(typeof a === "function");
}

primIsBool = function(a) {
   return PrimMkBool(typeof a === "boolean" || _primIsA(a, Boolean));
}

_primIsNumber = function(a) {
   return typeof a === "number" || _primIsA(a, Number);
}

primIsNumber = function(a) {
   return PrimMkBool(_primIsNumber(a));
}

_primIsString = function(a) {
   return typeof a === "string" || _primIsA(a, String);
}

primIsString = function(a) {
   return PrimMkBool(_primIsString(a));
}

primIsChar = function(a) {
   return PrimMkBool(_primIsString(a) && a.length == 1);
}

primIsInt = function(a) {
   return PrimMkBool(_primIsNumber(a) && parseFloat(a) == parseInt(a, 10) && !isNaN(a));
}

primIsDouble = function(a) {
   return PrimMkBool(_primIsNumber(a) && parseFloat(a) != parseInt(a, 10) && !isNaN(a));
}

primIsNull = function(a) {
   //typeof does not work, known bug.
   return PrimMkBool(a === null);
}

primIsUndefined = function(a) {
   return PrimMkBool(typeof a === "undefined");
}

primIsObject = function(a) {
   return PrimMkBool(typeof a === "object" && a !== null);
}

_primIsA = function(a, b) {
   //if a isObject and b isFunction
   if(typeof a === "object" && a !== null && typeof b === "function") {
      return a.constructor == b;
   }
   return false;
}

primIsA = function(a, b) {
   return PrimMkBool(_primIsA(a,b));
}

primIsInstanceOf = function(a, b) {
   if(typeof a === "object" && typeof b === "function") {
      return PrimMkBool(a instanceof b);
   }
   return PrimMkBool(false);
}

primEq = function(a, b) { 
  return PrimMkBool(a == b); 
}

function bar(a) {
	return "hello world";
}

function getNodeType(a) {
	if(a === null) {
		return -1;
	}
	return a.nodeType;
}

function getNodeOrNull(b) {
   if(b) {
      return document.body;
   } else {
      return null;
   }
}

function twice(f) {
	f();
	f();
}

function hof1(f) {
	alert(f(true));
	return "done";
}

function createCounter() {
  var i = 0;
  return function() {
    return i++;
  }
}

function callMinus() {
}

x = 0;
function mutX() {
   x += 10;
}
