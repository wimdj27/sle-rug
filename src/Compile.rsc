module Compile

import AST;
import Resolve;
import Eval;
import IO;
import lang::html5::DOM; // see standard library

/*
 * Implement a compiler for QL to HTML and Javascript
 *
 * - assume the form is type- and name-correct
 * - separate the compiler in two parts form2html and form2js producing 2 files
 * - use string templates to generate Javascript
 * - use the HTML5Node type and the `str toString(HTML5Node x)` function to format to string
 * - use any client web framework (e.g. Vue, React, jQuery, whatever) you like for event handling
 * - map booleans to checkboxes, strings to textfields, ints to numeric text fields
 * - be sure to generate uneditable widgets for computed questions!
 * - if needed, use the name analysis to link uses to definitions
 */
 
HTML5Attr vmodel(value val) = html5attr("v-model", val);
HTML5Attr vif(value val) = html5attr("v-if", val);

void compile(AForm f) {
  writeFile(f.src[extension="js"].top, form2js(f));
  writeFile(f.src[extension="html"].top, toString(form2html(f)));
}

HTML5Node form2html(AForm f) {
  return html(
    script(src("https://cdn.jsdelivr.net/npm/vue")),
    head( 
      title(f.name)
    ),
    body(
      h2(f.name),
      div(
        id("form"),
        form(
          [ question2html(q, f) | q <- f.questions ],
          input(
            \type("submit"),
            \value("Submit")
          )
        )
      )
    )
 );
}

HTML5Node question2html(AQuestion q, AForm f) {
  switch (q) {
    case regular(str l, str i, AType typ, src = loc d):
      switch (typ) {
        case string(): return html(p(label(l), input(\type("text"), id(i))));
        
        case integer(): return html(p(label(l), input(\type("number"), id(i))));
        
        case boolean(): return html(p(label(l), input(\type("checkbox"), id(i))));
      }
      
    case computed(str l, str id, AType typ, AExpr expr, src = loc d):
      return html(p(label(l), {{ /* value of expression */ }}));
      
    case qlist(list[AQuestion] questions, src = loc d):
      for (AQuestion question <- questions) question2html(question);
      
    case ifthenelse(AExpr expr, list[AQuestion] ifqs, list[AQuestion] elseqs, src = loc d): 
      return html(
        p(
          vif(expression2js(expr, f)),
          [ question2html(question, f) | question <- ifqs ]
        ),
        p(
          velse(),
          [ question2html(question, f) | question <- elseqs ]
        )
      );
    
    case ifthen(AExpr expr, list[AQuestion] ifqs): 
      return html(
        p(
          vif(expression2js(expr, f)),
          [ question2html(question, f) | question <- ifqs ]
        )
      );
            
    default: return html();
  }
}

str form2js(AForm f) {
  return "";
}

str expression2js(AExpr e, AForm f) {
  return "";
}
