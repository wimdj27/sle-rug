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
      tenv += <def, id, label, toType(typ)>;
    
    case computed(str label, str id, AType typ, AExpr expr, src = loc def):
      tenv += <def, id, label, toType(typ)>;
  }
  
  return tenv;
}

Type toType(AType t) {
  switch (t) {
    case string(): return tstr();
    case integer(): return tint();
    case boolean(): return tbool();
    default: return tunknown();
  }
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
      if ( (<_, x, _, tint()> <- tenv && <_, x, _, tbool()> <- tenv )
        || (<_, x, _, tint()> <- tenv && <_, x, _, tstr()> <- tenv )
        || (<_, x, _, tbool()> <- tenv && <_, x, _, tstr()> <- tenv )) {
        msgs += { error("Duplicate question with different types", u) };
      }
      
      if( <src1, _, label, _> <- tenv && <src2, _, label, _> <- tenv && src1 != src2) {
      	msgs += { warning("Duplicate label", u) };
  	  } 
      
    }
    
    case computed(str label, str id, AType typ, AExpr expr, src = loc u): {
     if ( (<_, x, _, tint()> <- tenv && <_, x, _, tbool()> <- tenv )
        || (<_, x, _, tint()> <- tenv && <_, x, _, tstr()> <- tenv )
        || (<_, x, _, tbool()> <- tenv && <_, x, _, tstr()> <- tenv )) {
        msgs += { error("Duplicate question with different types", u) };
     }
      
     if( <src1, _, label, _> <- tenv && <src2, _, label, _> <- tenv && src1 != src2) {
      	msgs += { warning("Duplicate label", u) };
   	 } 
  
      
      msgs += check(expr, tenv, useDef);
      
      msgs += { error("Declared type does not match expression type", u) | 
                typeOf(expr, tenv, useDef) != toType(typ) };
    }
    
    case qlist(list[AQuestion] questions, src = loc u): {
      for (q <- questions) msgs += check(q, tenv, useDef);
    }
    
    case ifthenelse(AExpr cond, list[AQuestion] ifqs, list[AQuestion] elseqs, src = loc u): {
      msgs += { error("Condition is not boolean", u) | typeOf(cond, tenv, useDef) != tbool() };
      msgs += check(cond, tenv, useDef);
      for (q <- ifqs) msgs += check(q, tenv, useDef);
      for (q <- elseqs) msgs += check(q, tenv, useDef);
    }
    
    case ifthen(AExpr cond, list[AQuestion] ifqs, src = loc u): {
      msgs += { error("Condition is not boolean", u) | typeOf(cond, tenv, useDef) != tbool() };
      msgs += check(cond, tenv, useDef);
      for (q <- ifqs) msgs += check(q, tenv, useDef);
    }
    
    default: return msgs;
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
      
    case multiplication(AExpr a, AExpr b, src = loc u): {
      typeA = typeOf(a, tenv, useDef);
      typeB = typeOf(b, tenv, useDef);
      msgs += { error("Type incompatibility", u) | typeA != typeB };
      msgs += { error("Type incompatibility", u) | typeA != tint() };
      msgs += check(a, tenv, useDef);
      msgs += check(b, tenv, useDef);
    }
      
    case division(AExpr a, AExpr b, src = loc u): {
      typeA = typeOf(a, tenv, useDef);
      typeB = typeOf(b, tenv, useDef);
      msgs += { error("Type incompatibility", u) | typeA != typeB };
      msgs += { error("Type incompatibility", u) | typeA != tint() };
      msgs += check(a, tenv, useDef);
      msgs += check(b, tenv, useDef);
    }
    
    case addition(AExpr a, AExpr b, src = loc u): {
      typeA = typeOf(a, tenv, useDef);
      typeB = typeOf(b, tenv, useDef);
      msgs += { error("Type incompatibility", u) | typeA != typeB };
      msgs += { error("Type incompatibility", u) | typeA != tint() };
      msgs += check(a, tenv, useDef);
      msgs += check(b, tenv, useDef);
    } 
      
    case subtraction(AExpr a, AExpr b, src = loc u): {
      typeA = typeOf(a, tenv, useDef);
      typeB = typeOf(b, tenv, useDef);
      msgs += { error("Type incompatibility", u) | typeA != typeB };
      msgs += { error("Type incompatibility", u) | typeA != tint() };
      msgs += check(a, tenv, useDef);
      msgs += check(b, tenv, useDef);
    }
      
    case greater(AExpr a, AExpr b, src = loc u): {
      typeA = typeOf(a, tenv, useDef);
      typeB = typeOf(b, tenv, useDef);
      msgs += { error("Type incompatibility", u) | typeA != typeB };
      msgs += { error("Type incompatibility", u) | typeA == tstr() };
      msgs += check(a, tenv, useDef);
      msgs += check(b, tenv, useDef);
    }
      
    case smaller(AExpr a, AExpr b, src = loc u): {
      typeA = typeOf(a, tenv, useDef);
      typeB = typeOf(b, tenv, useDef);
      msgs += { error("Type incompatibility", u) | typeA != typeB };
      msgs += { error("Type incompatibility", u) | typeA == tstr() };
      msgs += check(a, tenv, useDef);
      msgs += check(b, tenv, useDef);
    }
      
    case greatereq(AExpr a, AExpr b, src = loc u): {
      typeA = typeOf(a, tenv, useDef);
      typeB = typeOf(b, tenv, useDef);
      msgs += { error("Type incompatibility", u) | typeA != typeB };
      msgs += { error("Type incompatibility", u) | typeA == tstr() };
      msgs += check(a, tenv, useDef);
      msgs += check(b, tenv, useDef);
    }
      
    case smallereq(AExpr a, AExpr b, src = loc u): {
      typeA = typeOf(a, tenv, useDef);
      typeB = typeOf(b, tenv, useDef);
      msgs += { error("Type incompatibility", u) | typeA != typeB };
      msgs += { error("Type incompatibility", u) | typeA == tstr() };
      msgs += check(a, tenv, useDef);
      msgs += check(b, tenv, useDef);
    }
      
    case not(AExpr a, src = loc u): {
      typeA = typeOf(a, tenv, useDef);
      msgs += { error("Type incompatibility", u) | typeA != tbool() };
      msgs += check(a, tenv, useDef);
    }
      
    case equal(AExpr a, AExpr b, src = loc u): {
      typeA = typeOf(a, tenv, useDef);
      typeB = typeOf(b, tenv, useDef);
      msgs += { error("Type incompatibility", u) | typeA != typeB };
      msgs += check(a, tenv, useDef);
      msgs += check(b, tenv, useDef);
    }
      
    case noteq(AExpr a, AExpr b, src = loc u): {
      typeA = typeOf(a, tenv, useDef);
      typeB = typeOf(b, tenv, useDef);
      msgs += { error("Type incompatibility", u) | typeA != typeB };
      msgs += check(a, tenv, useDef);
      msgs += check(b, tenv, useDef);
    }
      
    case and(AExpr a, AExpr b, src = loc u): {
      typeA = typeOf(a, tenv, useDef);
      typeB = typeOf(b, tenv, useDef);
      msgs += { error("Type incompatibility", u) | typeA != typeB };
      msgs += { error("Type incompatibility", u) | typeA != tbool() };
      msgs += check(a, tenv, useDef);
      msgs += check(b, tenv, useDef);
    }
      
    case or(AExpr a, AExpr b, src = loc u): {
      typeA = typeOf(a, tenv, useDef);
      typeB = typeOf(b, tenv, useDef);
      msgs += { error("Type incompatibility", u) | typeA != typeB };
      msgs += { error("Type incompatibility", u) | typeA != tbool() };
      msgs += check(a, tenv, useDef);
      msgs += check(b, tenv, useDef);
    }
    
    default: return msgs;
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
      
    case boolean(bool bl):
      return tbool();
    
    case string(str s):
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
  
  return tunknown();
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
 
 
 
 
 