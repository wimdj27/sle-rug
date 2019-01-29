module CST2AST

import Syntax;
import AST;

import ParseTree;
import String;
import Boolean;

/*
 * Implement a mapping from concrete syntax trees (CSTs) to abstract syntax trees (ASTs)
 *
 * - Use switch to do case distinction with concrete patterns (like in Hack your JS) 
 * - Map regular CST arguments (e.g., *, +, ?) to lists 
 *   (NB: you can iterate over * / + arguments using `<-` in comprehensions or for-loops).
 * - Map lexical nodes to Rascal primitive types (bool, int, str)
 * - See the ref example on how to obtain and propagate source locations.
 */
  
AForm cst2ast(start[Form] sf) {
  f = sf.top; // remove layout before and after form
  
  switch (f) {
    case (Form) `form <Id x> { <Question* qs> }`:
      return form("<x>", [ cst2ast(q) | Question q <- qs ], src=f@\loc); 
    
    default: throw "Invalid form: <f>";
  }
}

AQuestion cst2ast(Question q) {
  switch (q) {
    case (Question) `<Str s> <Id x> : <Type t>`:
      return regular("<s>", "<x>", cst2ast(t), src=q@\loc, idsrc=x@\loc);
    
    case (Question) `<Str s> <Id x> : <Type t> = <Expr e>`:
      return computed("<s>", "<x>", cst2ast(t), cst2ast(e), src=q@\loc, idsrc=x@\loc);
      
    case (Question) `{ <Question* qs> }`:
      return qlist([ cst2ast(q) | Question q <- qs ], src=q@\loc);
      
    case (Question) `if ( <Expr cond> ) { <Question* ifqs> } else { <Question* elseqs> }`:
      return ifthenelse(cst2ast(cond), [ cst2ast(q) | Question q <- ifqs ], 
        [ cst2ast(q) | Question q <- elseqs ],  src=q@\loc);
    
    case (Question) `if ( <Expr cond> ) { <Question* qs> }`:
      return ifthen(cst2ast(cond), [ cst2ast(q) | Question q <- qs ], src=q@\loc);
      
    case (Question) `// _`:
      return empty("");
      
    default: throw "Invalid question: <q>";
  }
}

AExpr cst2ast(Expr e) {
  switch (e) {
    case (Expr) `( <Expr l> )`:
      return parentheses(cst2ast(l), src=e@\loc);
    
    case (Expr) `<Id x>`: 
      return ref("<x>", src=x@\loc);
      
    case (Expr) `<Int i>`:
      return integer(toInt("<i>"), src=i@\loc);
      
    case (Expr) `<Bool b>`:
      return boolean(fromString("<b>"), src=b@\loc);
      
    case (Expr) `<Str s>`:
      return string("<s>", src=s@\loc);
      
    case (Expr) `<Expr l> * <Expr r>`:
      return multiplication(cst2ast(l), cst2ast(r), src=e@\loc);
      
    case (Expr) `<Expr l> / <Expr r>`:
      return division(cst2ast(l), cst2ast(r), src=e@\loc);
      
    case (Expr) `<Expr l> + <Expr r>`:
      return addition(cst2ast(l), cst2ast(r), src=e@\loc);
      
    case (Expr) `<Expr l> - <Expr r>`:
      return subtraction(cst2ast(l), cst2ast(r), src=e@\loc);
      
    case (Expr) `<Expr l> \> <Expr r>`:
      return greater(cst2ast(l), cst2ast(r), src=e@\loc);
      
    case (Expr) `<Expr l> \< <Expr r>`:
      return smaller(cst2ast(l), cst2ast(r), src=e@\loc);
      
    case (Expr) `<Expr l> \>= <Expr r>`:
      return greatereq(cst2ast(l), cst2ast(r), src=e@\loc);
      
    case (Expr) `<Expr l> \<= <Expr r>`:
      return smallereq(cst2ast(l), cst2ast(r), src=e@\loc);
      
    case (Expr) `! <Expr l>`:
      return not(cst2ast(l), src=l@\loc);
      
    case (Expr) `<Expr l> == <Expr r>`:
      return equal(cst2ast(l), cst2ast(r), src=e@\loc);
      
    case (Expr) `<Expr l> != <Expr r>`:
      return noteq(cst2ast(l), cst2ast(r), src=e@\loc);
      
    case (Expr) `<Expr l> && <Expr r>`:
      return and(cst2ast(l), cst2ast(r), src=e@\loc);
      
    case (Expr) `<Expr l> || <Expr r>`:
      return or(cst2ast(l), cst2ast(r), src=e@\loc);
        
    default: throw "Invalid expression: <e>";
  }
}

AType cst2ast(Type t) {
  switch (t) {
  	case (Type) `boolean`:
  	  return boolean();
  	  
  	case (Type) `integer`:
  	  return integer();
  	  
  	case (Type) `string`:
  	  return string();
  	  
  	default: throw "Unknown type: <t>";
  }
}
