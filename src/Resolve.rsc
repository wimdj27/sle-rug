module Resolve

import AST;

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
    
    case ifthenelse(AExpr expr, list[AQuestion] ifqs, list[AQuestion] elseqs, src = loc u): {
      use += uses(expr);
      use += { uses(question) | question <- ifqs };
      use += { uses(question) | question <- elseqs };
    }
    
    case ifthen(AExpr expr, list[AQuestion] ifqs, src = loc u): {
      use += uses(expr);
      use += { uses(question) | question <- ifqs };
    }
    
    default: return;
  }
  
  return use;
}

Use uses(AExpr e) {
  Use use = {};
  
  switch(e) {
    case ref(str name, src = loc u):
      use += { <u, name> };
      
    case integer(AExpr a):
      use += uses(a);
      
    case boolean(AExpr a):
      use += uses(a);
    
    case string(AExpr a):
      use += uses(a);
    
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
      def += { defs(question) | question <- questions };
      
    case ifthenelse(AExpr expr, list[AQuestion] ifqs, list[AQuestion] elseqs, src = loc d): {
      def += { defs(question) | question <- ifqs };
      def += { defs(question) | question <- elseqs };
    }
    
    case ifthen(AExpr expr, list[AQuestion] ifqs): 
      def += { defs(question) | question <- ifqs };
      
    default: return;
  }
  
  return def;
}

