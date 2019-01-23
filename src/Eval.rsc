module Eval

import AST;
import Resolve;

/*
 * Implement big-step semantics for QL
 */
 
// NB: Eval may assume the form is type- and name-correct.


// Semantic domain for expressions (values)
data Value
  = vint(int n)
  | vbool(bool b)
  | vstr(str s)
  ;

// The value environment
alias VEnv = map[str name, Value \value];

// Modeling user input
data Input = input(str question, Value \value);
  
// produce an environment which for each question has a default value
// (e.g. 0 for int, "" for str etc.)
VEnv initialEnv(AForm f) {
  VEnv venv = ();
  
  for (q <- f.questions) {
    switch (q) {
      case regular(str label, str id, AType typ, src = loc def): {
        switch (typ) {
          case tint(): 
            venv += (id : vint(0));
            
          case tbool():
            venv += (id : vbool(false));
            
          case tstr():
            venv += (id : vstr(""));
          
          default: return venv;
        }
      }
    
      case computed(str label, str id, AType typ, AExpr expr, src = loc def): {
        switch (typ) {
          case tint(): 
            venv += (id : vint(0));
            
          case tbool():
            venv += (id : vbool(false));
            
          case tstr():
            venv += (id : vstr(""));
          
          default: return venv;
        }
      }
    }
  }
  
  return venv;
}


// Because of out-of-order use and declaration of questions
// we use the solve primitive in Rascal to find the fixpoint of venv.
VEnv eval(AForm f, Input inp, VEnv venv) {
  return solve (venv) {
    venv = evalOnce(f, inp, venv);
  }
}

VEnv evalOnce(AForm f, Input inp, VEnv venv) {
  for (q <- f.questions) {
    venv = eval(q, inp, venv);
  }
  
  return venv;
}

VEnv eval(AQuestion q, Input inp, VEnv venv) {
  // evaluate conditions for branching,
  // evaluate inp and computed questions to return updated VEnv
  
  switch (q) {
    case regular(str label, str id, AType typ, src = loc u):
      if (id == inp.question) venv[inp.question] = inp.\value;
    
    case computed(str label, str id, AType typ, AExpr expr, src = loc u):
      venv[id] = eval(expr, venv);
    
    case qlist(list[AQuestion] questions, src = loc u): {
      for (q <- questions) venv = eval(q, inp, venv);
    }
    
    case ifthenelse(AExpr cond, list[AQuestion] ifqs, list[AQuestion] elseqs, src = loc u): {
      if (eval(cond, venv).b) {
        for (q <- ifqs) venv = eval(q, inp, venv);
      } else {
        for (q <- elseqs) venv = eval(q, inp, venv);
      }
    }
    
    case ifthen(AExpr cond, list[AQuestion] ifqs, src = loc u): {
      if (eval(cond, venv).b) {
        for (q <- ifqs) venv = eval(q, inp, venv);
      }
    }
    
    default: return venv;
  }
  
  return venv;
}

Value eval(AExpr e, VEnv venv) {
  switch (e) {
    case parentheses(AExpr a):
      return eval(a);
    
    case ref(str x): return venv[x];
    
    case integer(int i):
      return vint(i);
      
    case boolean(bool bl):
      return vbool(bl);
    
    case string(str s):
      return vstr(s);
    
    case multiplication(AExpr a, AExpr b):
      return vint( eval(a, venv).n * eval(b, venv).n );
    
    case division(AExpr a, AExpr b): 
      return vint( eval(a, venv).n / eval(b, venv).n );
    
    case addition(AExpr a, AExpr b): 
      return vint( eval(a, venv).n + eval(b, venv).n );
    
    case subtraction(AExpr a, AExpr b): 
      return vint( eval(a, venv).n - eval(b, venv).n );
    
    case greater(AExpr a, AExpr b): 
      return vbool( eval(a, venv).b > eval(b, venv).b );
    
    case smaller(AExpr a, AExpr b): 
      return vbool( eval(a, venv).b < eval(b, venv).b );
    
    case greatereq(AExpr a, AExpr b):
      return vbool( eval(a, venv).b >= eval(b, venv).b );
    
    case smallereq(AExpr a, AExpr b): 
      return vbool( eval(a, venv).b <= eval(b, venv).b );
    
    case not(AExpr a):
      return vbool( !eval(a, venv).b );
    
    case equal(AExpr a, AExpr b):
      return vbool( eval(a, venv).b == eval(b, venv).b ); 
    
    case noteq(AExpr a, AExpr b): 
      return vbool( eval(a, venv).b != eval(b, venv).b );
    
    case and(AExpr a, AExpr b): 
      return vbool( eval(a, venv).b && eval(b, venv).b );
    
    case or(AExpr a, AExpr b): 
      return vbool( eval(a, venv).b || eval(b, venv).b );
    
    default: throw "Unsupported expression <e>";
  }
}