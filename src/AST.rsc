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
  = regular(str label, str id, AType typ)
  | computed(str label, str id, AType typ, AExpr expr)
  | qlist(list[AQuestion] questions)
  | ifthenelse(AExpr cond, list[AQuestion] ifqs, list[AQuestion] elseqs)
  | ifthen(AExpr cond, list[AQuestion] ifqs)
  | empty(str empty)
  ;

data AExpr(loc src = |tmp:///|)
  = ref(str name)
  | integer(int i)
  | boolean(bool bl)
  | string(str s)
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
  = string()
  | integer()
  | boolean()
  ;
