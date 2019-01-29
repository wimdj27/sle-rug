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

data AQuestion(loc src = |tmp:///|, loc idsrc = |tmp:///|)
  = regular(str label, str id, AType \type)
  | computed(str label, str id, AType \type, AExpr expr)
  | qlist(list[AQuestion] questions)
  | ifthenelse(AExpr cond, list[AQuestion] ifqs, list[AQuestion] elseqs)
  | ifthen(AExpr cond, list[AQuestion] ifqs)
  ;

data AExpr(loc src = |tmp:///|)
  = parentheses(AExpr l)
  | ref(str name)
  | integer(int i)
  | boolean(bool bl)
  | string(str s)
  | multiplication(AExpr l, AExpr r)
  | division(AExpr l, AExpr r)
  | addition(AExpr l, AExpr r)
  | subtraction(AExpr l, AExpr r)
  | greater(AExpr l, AExpr r)
  | smaller(AExpr l, AExpr r)
  | greatereq(AExpr l, AExpr r)
  | smallereq(AExpr l, AExpr r)
  | not(AExpr l)
  | equal(AExpr l, AExpr r)
  | noteq(AExpr l, AExpr r)
  | and(AExpr l, AExpr r)
  | or(AExpr l, AExpr r)
  ;

data AType(loc src = |tmp:///|)
  = string()
  | integer()
  | boolean()
  ;
