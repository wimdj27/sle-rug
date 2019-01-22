module Resolve

import AST;
import util::Math;
import Boolean;

/*
 * Name resolution for QL
 */ 


// modeling declaring occurrences of names
alias Def = rel[str name, loc def];

// modeling use occurrences of names
alias Use = rel[loc use, str name];

// the reference graph
alias UseDef = rel[loc use, loc def];

UseDef resolve(AForm f) = uses(f) o defs(f);

Use uses(AForm f) {
  Use use = {};
  
  for (q <- f.questions) {
    use += uses(q);
  }
  
  return use;
}

Use uses(AQuestion q) {
  Use use = {};
  
  switch (q) {
    case computed(str label, str id, AType typ, AExpr expr, src = loc u):
      use += uses(expr);
    
    case ifthenelse(AExpr cond, list[AQuestion] ifqs, list[AQuestion] elseqs, src = loc u): {
      use += uses(cond);
      for (qt <- ifqs) use += uses(qt);
      for (qt <- elseqs) use += uses(qt);
    }
    
    case ifthen(AExpr cond, list[AQuestion] ifqs, src = loc u): {
      use += uses(cond);
      for (qt <- ifqs) use += uses(qt);
    }
    
    default: return use;
  }
  
  return use;
}

Use uses(AExpr e) {
  Use use = {};
  
  switch(e) {
    case parentheses(AExpr a, src = loc u):
      use += uses(a);
    
    case ref(str name, src = loc u):
      use += { <u, name> };
    
    case multiplication(AExpr a, AExpr b): {
      use += uses(a);
      use += uses(b);
    }
    
    case division(AExpr a, AExpr b): {
      use += uses(a);
      use += uses(b);
    }
    
    case addition(AExpr a, AExpr b): {
      use += uses(a);
      use += uses(b);
    }
    
    case subtraction(AExpr a, AExpr b): {
      use += uses(a);
      use += uses(b);
    }
    
    case greater(AExpr a, AExpr b): {
      use += uses(a);
      use += uses(b);
    }
    
    case smaller(AExpr a, AExpr b): {
      use += uses(a);
      use += uses(b);
    }
    
    case greatereq(AExpr a, AExpr b): {
      use += uses(a);
      use += uses(b);
    }
    
    case smallereq(AExpr a, AExpr b): {
      use += uses(a);
      use += uses(b);
    }
    
    case not(AExpr a):
      use += uses(a);
    
    case equal(AExpr a, AExpr b): {
      use += uses(a);
      use += uses(b);
    }
    
    case noteq(AExpr a, AExpr b): {
      use += uses(a);
      use += uses(b);
    }
    
    case and(AExpr a, AExpr b): {
      use += uses(a);
      use += uses(b);
    }
    
    case or(AExpr a, AExpr b): {
      use += uses(a);
      use += uses(b);
    }
    
    default: return use;
  }
  
  return use;
}

Def defs(AForm f) {
  Def def = {};
  
  for (q <- f.questions) {
    def += defs(q);
  }
  
  return def;
}

Def defs(AQuestion q) {
  Def def = {};
  
  switch (q) {
    case regular(str label, str id, AType typ, src = loc d):
      def += { <id, d> };
      
    case computed(str label, str id, AType typ, AExpr expr, src = loc d):
      def += { <id, d> };
      
    case qlist(list[AQuestion] questions, src = loc d):
      for (AQuestion qt <- questions) def += defs(qt);
      
    case ifthenelse(AExpr expr, list[AQuestion] ifqs, list[AQuestion] elseqs, src = loc d): {
      for (AQuestion qt <- ifqs) def += defs(qt);
      for (AQuestion qt <- elseqs) def += defs(qt);
    }
    
    case ifthen(AExpr expr, list[AQuestion] ifqs): 
      for (AQuestion qt <- ifqs) def += defs(qt);
      
    default: return def;
  }
  
  return def;
}

