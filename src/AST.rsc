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
  = regular(str name, str id, AType typ)
  | computed(str name, str id, AType typ, AExpr expr)
  | qlist(list[AQuestion] questions)
  | ifthenelse(list[AQuestion] ifqs, list[AQuestion] elseqs)
  | ifthen(list[AQuestion] ifqs)
  | empty(str empty)
  ;

data AExpr(loc src = |tmp:///|)
  = ref(str name)
  | integer(AExpr a)
  | boolean(AExpr a)
  | string(AExpr a)
  | multiplication(AExpr a, AExpr b)
  | division(AExpr a, AExpr b)
  | addition(AExpr a, AExpr b)
  | subtraction(AExpr a, AExpr b)
  | greater(AExpr a, AExpr b)
  | smaller(AExpr a, AExpr b)
  | greatereq(AExpr a, AExpr b)
  | smallereq(AExpr a, AExpr b)
  | not(AExpr a)
  | equal(AExpr a, AExpr b)
  | noteq(AExpr a, AExpr b)
  | and(AExpr a, AExpr b)
  | or(AExpr a, AExpr b)
  ;

data AType(loc src = |tmp:///|)
  = boolean()
  | integer()
  ;
