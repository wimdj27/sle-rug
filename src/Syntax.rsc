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
  = 
  ; 

// TODO: +, -, *, /, &&, ||, !, >, <, <=, >=, ==, !=, literals (bool, int, str)
// Think about disambiguation using priorities and associativity
// and use C/Java style precedence rules (look it up on the internet)
syntax Expr 
  = Id \ "true" \ "false" // true/false are reserved keywords.
  > assoc (left Expr "*" Expr | left Expr "/" Expr)
  > assoc (left Expr "+" Expr | left Expr "-" Expr)
  > assoc (Expr "\>" Expr | Expr "\<" Expr | Expr "\>=" Expr | Expr "\<=" Expr)
  > "!" Expr
  > assoc (left Expr "==" Expr | left Expr "!=" Expr)
  > assoc (left Expr "&&" Expr | left Expr "||" Expr)
  ;
  
syntax Type
  = Int
  | Str;  
  
lexical Str = ;

lexical Int 
  = ;

lexical Bool = "true" | "false";



