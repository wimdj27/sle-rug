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
  
  for (q <- f.questions)
    use += uses(q);
  
  return use;
}

Use uses(AQuestion q) {
  Use use = {};
  
  switch (q) {
    case computed(str label, str id, AType \type, AExpr expr, src = loc u):
      use += uses(expr);
    
    case ifthenelse(AExpr cond, list[AQuestion] ifqs, list[AQuestion] elseqs, src = loc u): {
      use += uses(cond);
      for (q2 <- ifqs) use += uses(q2);
      for (q2 <- elseqs) use += uses(q2);
    }
    
    case ifthen(AExpr cond, list[AQuestion] ifqs, src = loc u): {
      use += uses(cond);
      for (q2 <- ifqs) use += uses(q2);
    }
    
    default: return use;
  }
  
  return use;
}

Use uses(AExpr e) {
  Use use = {};
  
  switch(e) {
    case parentheses(AExpr l, src = loc u):
      use += uses(l);
    
    case ref(str name, src = loc u):
      use += { <u, name> };
    
    case multiplication(AExpr l, AExpr r): {
      use += uses(l);
      use += uses(r);
    }
    
    case division(AExpr l, AExpr r): {
      use += uses(l);
      use += uses(r);
    }
    
    case addition(AExpr l, AExpr r): {
      use += uses(l);
      use += uses(r);
    }
    
    case subtraction(AExpr l, AExpr r): {
      use += uses(l);
      use += uses(r);
    }
    
    case greater(AExpr l, AExpr r): {
      use += uses(l);
      use += uses(r);
    }
    
    case smaller(AExpr l, AExpr r): {
      use += uses(l);
      use += uses(r);
    }
    
    case greatereq(AExpr l, AExpr r): {
      use += uses(l);
      use += uses(r);
    }
    
    case smallereq(AExpr l, AExpr r): {
      use += uses(l);
      use += uses(r);
    }
    
    case not(AExpr l):
      use += uses(l);
    
    case equal(AExpr l, AExpr r): {
      use += uses(l);
      use += uses(r);
    }
    
    case noteq(AExpr l, AExpr r): {
      use += uses(l);
      use += uses(r);
    }
    
    case and(AExpr l, AExpr r): {
      use += uses(l);
      use += uses(r);
    }
    
    case or(AExpr l, AExpr r): {
      use += uses(l);
      use += uses(r);
    }
    
    default: return use;
  }
  
  return use;
}

Def defs(AForm f) {
  Def def = {};
  
  for (q <- f.questions)
    def += defs(q);
  
  return def;
}

Def defs(AQuestion q) {
  Def def = {};
  
  switch (q) {
    case regular(str label, str id, AType \type, src = loc d, idsrc = loc d2):
      def += { <id, d2> };
      
    case computed(str label, str id, AType \type, AExpr expr, src = loc d, idsrc = loc d2):
      def += { <id, d2> };
      
    case qlist(list[AQuestion] questions, src = loc d):
      for (AQuestion q2 <- questions) def += defs(q2);
      
    case ifthenelse(AExpr expr, list[AQuestion] ifqs, list[AQuestion] elseqs, src = loc d): {
      for (AQuestion q2 <- ifqs) def += defs(q2);
      for (AQuestion q2 <- elseqs) def += defs(q2);
    }
    
    case ifthen(AExpr expr, list[AQuestion] ifqs): 
      for (AQuestion q2 <- ifqs) def += defs(q2);
      
    default: return def;
  }
  
  return def;
}

