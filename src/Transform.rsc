module Transform

import Syntax;
import Resolve;
import AST;
extend lang::std::Id;

/* 
 * Transforming QL forms
 */
 
 
/* Normalization:
 *  wrt to the semantics of QL the following
 *     q0: "" int; 
 *     if (a) { 
 *        if (b) { 
 *          q1: "" int; 
 *        } 
 *        q2: "" int; 
 *      }
 *
 *  is equivalent to
 *     if (true) q0: "" int;
 *     if (a && b) q1: "" int;
 *     if (a) q2: "" int;
 *
 * Write a transformation that performs this flattening transformation.
 *
 */
 
AForm flatten(AForm f) {
  int i = 0;
  for (q <- f.questions) {
    f.questions[i] = flatten(q, boolean(true));
    i += 1;
  }
  return f;
}

AQuestion flatten(AQuestion q, AExpr condition) {
  switch (q) {
    case regular(str label, str id, AType typ, src = loc d):
      return ifthen(condition, [q]);
      
    case computed(str label, str id, AType typ, AExpr expr, src = loc d):
      return ifthen(condition, [q]);
      
    case qlist(list[AQuestion] questions, src = loc d):
      return qlist([flatten(q2, condition) | AQuestion q2 <- questions]); 
      
    case ifthenelse(AExpr cond, list[AQuestion] ifqs, list[AQuestion] elseqs, src = loc d): 
      return ifthenelse(and(cond, condition), [flatten(q2, and(cond, condition)) | AQuestion q2 <- ifqs], 
        [flatten(q3, and(not(cond), condition)) | AQuestion q3 <- elseqs]);
    
    case ifthen(AExpr cond, list[AQuestion] ifqs): 
      return ifthen(and(cond, condition), [flatten(q2, and(cond, condition)) | AQuestion q2 <- ifqs]);
            
    default: return;
  }
}

/* Rename refactoring:
 *
 * Write a refactoring transformation that consistently renames all occurrences of the same name.
 * Use the results of name resolution to find the equivalence class of a name.
 *
 */
 
start[Form] rename(start[Form] f, loc useOrDef, str newName, UseDef useDef) {
  set[loc] locs = { u | <loc u, loc d> <- useDef, d == useOrDef } 
    + { d | <loc u, loc d> <- useDef, u == useOrDef }  + useOrDef;
  
  return visit (f) {
    case Id x => [Id]newName
      when x@\loc in locs
  }
}

 
 

