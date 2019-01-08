module AST

/*
 * Define Abstract Syntax for QL
 *
 * - complete the following data types
 * - make sure there is an almost one-to-one correspondence with the grammar
 */

data AForm(loc src = |tmp:///|)
  = form(str name, list[AQuestion] questions)
  ; 

data AQuestion(loc src = |tmp:///|)
  = question(str name, str id, str typ, AExpr expr)
  ;

data AExpr(loc src = |tmp:///|)
  = ref(str name)
  ;

data AType(loc src = |tmp:///|)
  = typ(str name)
  ;
