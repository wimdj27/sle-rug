module Check

import AST;
import Resolve;
import Message; // see standard library

data Type
  = tint()
  | tbool()
  | tstr()
  | tunknown()
  ;

// the type environment consisting of defined questions in the form 
alias TEnv = rel[loc def, str name, str label, Type \type];

// To avoid recursively traversing the form, use the `visit` construct
// or deep match (e.g., `for (/question(...) := f) {...}` ) 
TEnv collect(AForm f) {
  TEnv tenv = {};
  
  visit(f) {
    case regular(str label, str id, AType typ, src = loc def):
      tenv += <def, name, label, tint()>;
  }
  
  return tenv;
}

set[Message] check(AForm f, TEnv tenv, UseDef useDef) {
  set[Message] msgs = {};
  
  for (q <- f.questions) {
    msgs += check(q, tenv, useDef);
  }
  
  return msgs;
}

// - produce an error if there are declared questions with the same name but different types.
// - duplicate labels should trigger a warning 
// - the declared type computed questions should match the type of the expression.
set[Message] check(AQuestion q, TEnv tenv, UseDef useDef) {
  set[Message] msgs = {};
  
  switch (q) {
    case regular(str label, str id, AType typ, src = loc u): {
      // msgs += { error("Duplicate question name with different type.", u) | tenv.type = true };
      ;
    }
  }
  
  return msgs;
}

// Check operand compatibility with operators.
// E.g. for an addition node add(lhs, rhs), 
//   the requirement is that typeOf(lhs) == typeOf(rhs) == tint()
set[Message] check(AExpr e, TEnv tenv, UseDef useDef) {
  set[Message] msgs = {};
  
  switch (e) {
    case ref(str x, src = loc u):
      msgs += { error("Undeclared question", u) | useDef[u] == {} };

    
  }
  
  return msgs; 
}

Type typeOf(AExpr e, TEnv tenv, UseDef useDef) {
  switch (e) {
    case ref(str x, src = loc u):  
      if (<u, loc d> <- useDef, <d, x, _, Type t> <- tenv) {
        return t;
      }
    
    case integer(int i):
      return tint();
      
    case boolean(AExpr a):
      return tbool();
    
    case string(AExpr a):
      return tstring();
    
    case multiplication(AExpr a, AExpr b):
      return tint();
    
    case division(AExpr a, AExpr b): 
      return tint();
    
    case addition(AExpr a, AExpr b): 
      return tint();
    
    case subtraction(AExpr a, AExpr b): 
      return tint();
    
    case greater(AExpr a, AExpr b): 
      return tbool();
    
    case smaller(AExpr a, AExpr b): 
      return tbool();
    
    case greatereq(AExpr a, AExpr b):
      return tbool();
    
    case smallereq(AExpr a, AExpr b): 
      return tbool();
    
    case not(AExpr a):
      return tbool();
    
    case equal(AExpr a, AExpr b):
      return tbool(); 
    
    case noteq(AExpr a, AExpr b): 
      return tbool();
    
    case and(AExpr a, AExpr b): 
      return tbool();
    
    case or(AExpr a, AExpr b): 
      return tbool();
    
    default: return tunknown();
  }
}

/* 
 * Pattern-based dispatch style:
 * 
 * Type typeOf(ref(str x, src = loc u), TEnv tenv, UseDef useDef) = t
 *   when <u, loc d> <- useDef, <d, x, _, Type t> <- tenv
 *
 * ... etc.
 * 
 * default Type typeOf(AExpr _, TEnv _, UseDef _) = tunknown();
 *
 */
 
 
 
 
 