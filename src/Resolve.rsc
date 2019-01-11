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
  
  for (Question q <- f.questions) {
    use += uses(q);
  }
  
  return use;  
}

Use uses(AQuestion q) {
  switch (q) {
      case computed(str label, str id, AType typ, AExpr expr, src = loc u): {
        Use use2 = {};
        switch (expr) {
          case ref(str name, src = loc u): 
            use2 += <u, name>;
          case integer(AExpr a): 
        }
        use += use2;
      }
    }
}

Use uses(AExpr e) {
  return {};
}

Def defs(AForm f) {
  return {};
}