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
      return regular("<s>", "<t>", "<x>", src=q@\loc);
    
    case (Question) `<Str s> <Id x> : <Type t> = <Expr e>`:
      return computed("<s>", "<t>", "<x>", cst2ast(e), src=q@\loc);
      
    case (Question) `{ <Question* qs> }`:
      return qlist([cst2ast(q) | Question q <- qs], src=q@\loc);
      
    case (Question) `if ( <Id x> ) { <Question* ifqs> } else { <Question* elseqs> }`:
      return ifthenelse([cst2ast(q) | Question q <- ifqs], [cst2ast(q) | Question q <- elseqs],  src=q@\loc);
    
    case (Question) `if ( <Id x> ) { <Question* qs> }`:
      return ifthen([cst2ast(q) | Question q <- qs], src=q@\loc);
     
    case (Question) ``: // empty question
      return empty("");
      
    case (Question) `// _`:
      return empty("");
      
    default: throw "Invalid question: <q>";
  }
}

AExpr cst2ast(Expr e) {
  switch (e) {
    case (Expr) `<Id x>`: 
      return ref("<x>", src=x@\loc);
      
    case (Expr) `<Int i>`:
      return integer(i, src=i@\loc);
      
    case (Expr) `<Bool b>`:
      return boolean(b, src=b@\loc);
      
    case (Expr) `<Str s>`:
      return string(s, src=s@\loc);
      
    case (Expr) `<Expr a> * <Expr b>`:
      return multiplication(cst2ast(a), cst2ast(b), src=e@\loc);
      
    case (Expr) `<Expr a> / <Expr b>`:
      return division(cst2ast(a), cst2ast(b), src=e@\loc);
      
    case (Expr) `<Expr a> + <Expr b>`:
      return addition(cst2ast(a), cst2ast(b), src=e@\loc);
      
    case (Expr) `<Expr a> - <Expr b>`:
      return subtraction(cst2ast(a), cst2ast(b), src=e@\loc);
      
    case (Expr) `<Expr a> \> <Expr b>`:
      return greater(cst2ast(a), cst2ast(b), src=e@\loc);
      
    case (Expr) `<Expr a> \< <Expr b>`:
      return smaller(cst2ast(a), cst2ast(b), src=e@\loc);
      
    case (Expr) `<Expr a> \>= <Expr b>`:
      return greatereq(cst2ast(a), cst2ast(b), src=e@\loc);
      
    case (Expr) `<Expr a> \<= <Expr b>`:
      return smallereq(cst2ast(a), cst2ast(b), src=e@\loc);
      
    case (Expr) `! <Expr a>`:
      return not(cst2ast(a), src=a@\loc);
      
    case (Expr) `<Expr a> == <Expr b>`:
      return equal(cst2ast(a), cst2ast(b), src=e@\loc);
      
    case (Expr) `<Expr a> != <Expr b>`:
      return noteq(cst2ast(a), cst2ast(b), src=e@\loc);
      
    case (Expr) `<Expr a> && <Expr b>`:
      return and(cst2ast(a), cst2ast(b), src=e@\loc);
      
    case (Expr) `<Expr a> || <Expr b>`:
      return or(cst2ast(a), cst2ast(b), src=e@\loc);
        
    default: throw "Unhandled expression: <e>";
  }
}

AType cst2ast(Type t) {
  switch (t) {
  	case (Type) `boolean`:
  	  return boolean();
  	  
  	case (Type) `integer`:
  	  return integer();
  	  
  	default: throw "Unknown type: <t>";
  }
}
