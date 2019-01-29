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
    case regular(str label, str id, AType typ, src = loc def, idsrc = def2):
      tenv += <def2, id, label, toType(typ)>;
    
    case computed(str label, str id, AType typ, AExpr expr, src = loc def, idsrc = def2):
      tenv += <def2, id, label, toType(typ)>;
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
  
  if ( (<loc1, x, _, tint()> <- tenv && <loc2, x, _, tbool()> <- tenv )
        || (<loc1, x, _, tint()> <- tenv && <loc2, x, _, tstr()> <- tenv )
        || (<loc1, x, _, tbool()> <- tenv && <loc2, x, _, tstr()> <- tenv )) {
        msgs += error("Duplicate question with different types", loc1);
        msgs += error("Duplicate question with different types", loc2);
  }
  
  for (AQuestion q <- f.questions) {
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
    case regular(str label, str id, AType \type, src = loc u):
      if( <src1, _, label, _> <- tenv && <src2, _, label, _> <- tenv && src1 != src2)
      	msgs += { warning("Duplicate label", u) };
    
    case computed(str label, str id, AType \type, AExpr expr, src = loc u): {
      if( <src1, _, label, _> <- tenv && <src2, _, label, _> <- tenv && src1 != src2)
      	msgs += { warning("Duplicate label", u) };
      
      msgs += check(expr, tenv, useDef);
      msgs += { error("Declared type does not match expression type", u) | 
                typeOf(expr, tenv, useDef) != toType(\type) };
    }
    
    case qlist(list[AQuestion] questions, src = loc u):
      for (AQuestion q <- questions) msgs += check(q, tenv, useDef);
    
    case ifthenelse(AExpr cond, list[AQuestion] ifqs, list[AQuestion] elseqs, src = loc u): {
      msgs += { error("Condition is not boolean", u) | typeOf(cond, tenv, useDef) != tbool() };
      msgs += check(cond, tenv, useDef);
      for (AQuestion q <- ifqs)   msgs += check(q, tenv, useDef);
      for (AQuestion q <- elseqs) msgs += check(q, tenv, useDef);
    }
    
    case ifthen(AExpr cond, list[AQuestion] ifqs, src = loc u): {
      msgs += { error("Condition is not boolean", u) | typeOf(cond, tenv, useDef) != tbool() };
      msgs += check(cond, tenv, useDef);
      for (AQuestion q <- ifqs) msgs += check(q, tenv, useDef);
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
    case parentheses(AExpr l, src = loc u): 
      msgs += check(l, tenv, useDef);
  
    case ref(str x, src = loc u):
      msgs += { error("Undeclared question", u) | useDef[u] == {} };
      
    case multiplication(AExpr l, AExpr r, src = loc u): {
      typeL = typeOf(l, tenv, useDef);
      typeR = typeOf(r, tenv, useDef);
      msgs += { error("Type incompatibility", u) | typeL != typeR };
      msgs += { error("Type incompatibility", u) | typeL != tint() };
      msgs += check(l, tenv, useDef);
      msgs += check(r, tenv, useDef);
    }
      
    case division(AExpr l, AExpr r, src = loc u): {
      typeL = typeOf(l, tenv, useDef);
      typeR = typeOf(r, tenv, useDef);
      msgs += { error("Type incompatibility", u) | typeL != typeR };
      msgs += { error("Type incompatibility", u) | typeL != tint() };
      msgs += check(l, tenv, useDef);
      msgs += check(r, tenv, useDef);
    }
    
    case addition(AExpr l, AExpr r, src = loc u): {
      typeL = typeOf(l, tenv, useDef);
      typeR = typeOf(r, tenv, useDef);
      msgs += { error("Type incompatibility", u) | typeL != typeR };
      msgs += { error("Type incompatibility", u) | typeL != tint() };
      msgs += check(l, tenv, useDef);
      msgs += check(r, tenv, useDef);
    } 
      
    case subtraction(AExpr l, AExpr r, src = loc u): {
      typeL = typeOf(l, tenv, useDef);
      typeR = typeOf(r, tenv, useDef);
      msgs += { error("Type incompatibility", u) | typeL != typeR };
      msgs += { error("Type incompatibility", u) | typeL != tint() };
      msgs += check(l, tenv, useDef);
      msgs += check(r, tenv, useDef);
    }
      
    case greater(AExpr l, AExpr r, src = loc u): {
      typeL = typeOf(l, tenv, useDef);
      typeR = typeOf(r, tenv, useDef);
      msgs += { error("Type incompatibility", u) | typeL != typeR };
      msgs += { error("Type incompatibility", u) | typeL == tstr() };
      msgs += check(l, tenv, useDef);
      msgs += check(r, tenv, useDef);
    }
      
    case smaller(AExpr l, AExpr r, src = loc u): {
      typeL = typeOf(l, tenv, useDef);
      typeR = typeOf(r, tenv, useDef);
      msgs += { error("Type incompatibility", u) | typeL != typeR };
      msgs += { error("Type incompatibility", u) | typeL == tstr() };
      msgs += check(l, tenv, useDef);
      msgs += check(r, tenv, useDef);
    }
      
    case greatereq(AExpr l, AExpr r, src = loc u): {
      typeL = typeOf(l, tenv, useDef);
      typeR = typeOf(r, tenv, useDef);
      msgs += { error("Type incompatibility", u) | typeL != typeR };
      msgs += { error("Type incompatibility", u) | typeL == tstr() };
      msgs += check(l, tenv, useDef);
      msgs += check(r, tenv, useDef);
    }
      
    case smallereq(AExpr l, AExpr r, src = loc u): {
      typeL = typeOf(l, tenv, useDef);
      typeR = typeOf(r, tenv, useDef);
      msgs += { error("Type incompatibility", u) | typeL != typeR };
      msgs += { error("Type incompatibility", u) | typeL == tstr() };
      msgs += check(l, tenv, useDef);
      msgs += check(r, tenv, useDef);
    }
      
    case not(AExpr l, src = loc u): {
      typeL = typeOf(l, tenv, useDef);
      msgs += { error("Type incompatibility", u) | typeL != tbool() };
      msgs += check(l, tenv, useDef);
    }
      
    case equal(AExpr l, AExpr r, src = loc u): {
      typeL = typeOf(l, tenv, useDef);
      typeR = typeOf(r, tenv, useDef);
      msgs += { error("Type incompatibility", u) | typeL != typeR };
      msgs += check(l, tenv, useDef);
      msgs += check(r, tenv, useDef);
    }
      
    case noteq(AExpr l, AExpr r, src = loc u): {
      typeL = typeOf(l, tenv, useDef);
      typeR = typeOf(r, tenv, useDef);
      msgs += { error("Type incompatibility", u) | typeL != typeR };
      msgs += check(l, tenv, useDef);
      msgs += check(r, tenv, useDef);
    }
      
    case and(AExpr l, AExpr r, src = loc u): {
      typeL = typeOf(l, tenv, useDef);
      typeR = typeOf(r, tenv, useDef);
      msgs += { error("Type incompatibility", u) | typeL != typeR };
      msgs += { error("Type incompatibility", u) | typeL != tbool() };
      msgs += check(l, tenv, useDef);
      msgs += check(r, tenv, useDef);
    }
      
    case or(AExpr l, AExpr r, src = loc u): {
      typeL = typeOf(l, tenv, useDef);
      typeR = typeOf(r, tenv, useDef);
      msgs += { error("Type incompatibility", u) | typeL != typeR };
      msgs += { error("Type incompatibility", u) | typeL != tbool() };
      msgs += check(l, tenv, useDef);
      msgs += check(r, tenv, useDef);
    }
  }
  
  return msgs; 
}

Type typeOf(AExpr e, TEnv tenv, UseDef useDef) {
  switch (e) {
    case ref(str x, src = loc u):  
      if (<u, loc d> <- useDef, <d, x, _, Type t> <- tenv) return t;
    
    case parentheses(AExpr expr): 
      return typeOf(expr, tenv, useDef);
    	
    case integer(int i):
      return tint();
      
    case boolean(bool bl):
      return tbool();
    
    case string(str s):
      return tstr();
    
    case multiplication(AExpr l, AExpr r):
      return tint();
    
    case division(AExpr l, AExpr r): 
      return tint();
    
    case addition(AExpr l, AExpr r): 
      return tint();
    
    case subtraction(AExpr l, AExpr r): 
      return tint();
    
    case greater(AExpr l, AExpr r): 
      return tbool();
    
    case smaller(AExpr l, AExpr r): 
      return tbool();
    
    case greatereq(AExpr l, AExpr r):
      return tbool();
    
    case smallereq(AExpr l, AExpr r): 
      return tbool();
    
    case not(AExpr l):
      return tbool();
    
    case equal(AExpr l, AExpr r):
      return tbool(); 
    
    case noteq(AExpr l, AExpr r): 
      return tbool();
    
    case and(AExpr l, AExpr r): 
      return tbool();
    
    case or(AExpr l, AExpr r): 
      return tbool();
    
    default: return tunknown();
  }
  
  return tunknown();
}
 
 
 
 
 