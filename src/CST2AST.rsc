module CST2AST

import Syntax;
import AST;

import ParseTree;
import String;

/*
 * Implement a mapping from concrete syntax trees (CSTs) to abstract syntax trees (ASTs)
 *
 * - Use switch to do case distinction with concrete patterns (like in Hack your JS) 
 * - Map regular CST arguments (e.g., *, +, ?) to lists 
 *   (NB: you can iterate over * / + arguments using `<-` in comprehensions or for-loops).
 * - Map lexical nodes to Rascal primitive types (bool, int, str)
 * - See the ref example on how to obtain and propagate source locations.
 */
  
AForm cst2ast(f: (Form) `form <Id x> { <Question* qs> }`) {
  f = sf.top; // remove layout before and after form
  return form("<x>", [ cst2ast(q) | Question q <- qs ], src=f@\loc); 
}

AQuestion cst2ast(Question q) {
  switch (q) {
    case (Question) `<Str s> <Id x> : <Type t>`:
      return question("<s>", "<t>", "<x>", null, src=q@\loc);
    
    case (Question) `<Str s> <Id x> : <Type t> = <Expr e>`:
      return question("<s>", "<t>", "<x>", cst2ast(e), src=q@\loc);
      
    case (Question) `{ <Question* qs> }`:
      for (q <- qs) cst2ast(q);
      
    case (Question) `if ( <Id x> ) { <Question* if_qs> } else { <Question* else_qs> }`: {
      for (q <- if_qs)   cst2ast(q);
      for (q <- else_qs) cst2ast(q);
    }
    
    case (Question) `if ( <Id x> ) { <Question* qs> }`:
      for (q <- qs) cst2ast(q);
     
    case (Question) ``: // empty question
      return;
      
    default: throw "Invalid question: <q>";
  }
}

AExpr cst2ast(Expr e) {
  switch (e) {
    case (Expr) `<Id x>`: 
      return ref("<x>", src=x@\loc);
      
    case (Expr) `<Int i>`:
      return ref("<i>", src=x@\loc);
      
    case (Expr) `<Bool b>`:
      return ref("<b>", src=x@\loc);
      
    case (Expr) `<Str s>`:
      return ref("<s>", src=x@\loc);
      
    case (Expr) `<Expr a> * <Expr b>`:
      return ;
        
    default: throw "Unhandled expression: <e>";
  }
}

AType cst2ast(Type t) {
  throw "Not yet implemented";
}
