module Syntax

extend lang::Layout;
extend lang::Id;

/*
 * Concrete syntax of QL
 */

start syntax Form 
  = "form" Id "{" Question* "}";

// TODO: question, computed question, block, if-then-else, if-then
syntax Question
  = Str ":" Type
  | Str ":" Type "=" Expr
  | "{" Question* "}"
  | "if" "(" Id ")" "{" Question* "}" "else" "{" Question* "}"
  | "if" "(" Id ")" "{" Question* "}"
  ; 

// TODO: +, -, *, /, &&, ||, !, >, <, <=, >=, ==, !=, literals (bool, int, str)
// Think about disambiguation using priorities and associativity
// and use C/Java style precedence rules (look it up on the internet)
syntax Expr 
  = Id \ "true" \ "false" // true/false are reserved keywords.
  | Int
  | Bool
  | Str
  > left (Expr "*" Expr | Expr "/" Expr)
  > left (Expr "+" Expr | Expr "-" Expr)
  > non-assoc (Expr "\>" Expr | Expr "\<" Expr | Expr "\>=" Expr | Expr "\<=" Expr)
  > "!" Expr
  > left (Expr "==" Expr | Expr "!=" Expr)
  > left (Expr "&&" Expr | Expr "||" Expr)
  ;
  
syntax Type
  = "boolean"
  | "integer";
  
lexical Str 
  = "\"" [a-zA-Z0-9_]* "\"";

lexical Int 
  = [0-9]*;

lexical Bool 
  = "true" 
  | "false";



