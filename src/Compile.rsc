module Compile

import AST;
import Resolve;
import Transform;
import Boolean;
import IO;
import util::Math;
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
  f = flatten(f);
  writeFile(f.src[extension="js"].top, form2js(f));
  writeFile(f.src[extension="html"].top, toString(form2html(f)));
}

HTML5Node form2html(AForm f) {
  return html(
    head( 
      title(f.name)
    ),
    body(
      h2(f.name),
      div(
        id("form"),
        form(
          div(
            [ question2html(q, f, "true") | q <- f.questions ]
          ),
          input(
            \type("submit"),
            \value("Submit " + f.name)
          )
        )
      ),
      script(
        src("https://unpkg.com/vue")
      ),
      script(
        src("https://unpkg.com/axios/dist/axios.min.js")
      ),
      script(
        src(f.src[extension="js"].top.file)
      )
    )
 );
}

HTML5Node question2html(AQuestion q, AForm f, str condition) {
  switch (q) {
    case regular(str l, str i, AType t, src = loc d):
      switch (t) {
        case string(): return p(label(l[1..-1]), input(\type("text"), vmodel(i)), vif(condition));
        
        case integer(): return p(label(l[1..-1]), input(\type("number"), vmodel(i)), vif(condition));
        
        case boolean(): return p(label(l[1..-1]), input(\type("checkbox"), vmodel(i)), vif(condition));
      }
      
    case computed(str l, str i, AType t, AExpr expr, src = loc d):
      return p(label(l[1..-1]), "{{ <i>() }}", vif(condition));
      
    case qlist(list[AQuestion] questions, src = loc d):
      return div(
        [ question2html(q2, f, "true") | AQuestion q2 <- questions ]
      );
      
    case ifthenelse(AExpr cond, list[AQuestion] ifqs, list[AQuestion] elseqs, src = loc d): 
      return div(
        div(
          [ question2html(q2, f, expression2js(cond, f)) | AQuestion q2 <- ifqs ]
        ),
        div(
          [ question2html(q2, f, ("!(" + expression2js(cond, f)) + ")") | AQuestion q2 <- elseqs ]
        )
      );
    
    case ifthen(AExpr cond, list[AQuestion] ifqs): 
      return div(
        [ question2html(q2, f, expression2js(cond, f)) | AQuestion q2 <- ifqs ]
      );
            
    default: return p();
  }
}

str initial(AQuestion q){
  switch (q) {
    case regular(str label, str id, AType typ, src = loc u):
      switch(typ){
        case integer(): return "\n        " + id + ": 0,";
        case boolean(): return "\n        " + id + ": false,";
        case string(): return "\n        " + id + ": \'\',";
      }
    
    case qlist(list[AQuestion] questions, src = loc u): {
      str new = "";
      for (AQuestion q2 <- questions) new += initial(q2);
      return new;
    }
    
    case ifthenelse(AExpr cond, list[AQuestion] ifqs, list[AQuestion] elseqs, src = loc u): {
      str new = "";
      for (AQuestion q2 <- ifqs) new += initial(q2);
      for (AQuestion q2 <- elseqs) new += initial(q2);
      return new;
    }
    
    case ifthen(AExpr cond, list[AQuestion] ifqs, src = loc u): {
      str new = "";
      for (AQuestion q2 <- ifqs) new += initial(q2);
      return new;
    }
    
    default: return "";
  }
}


str form2js(AForm f) {
  str script = "var form = new Vue({
 			   '    el: \'#form\',
 			   '    data: {";
 			 
  for (AQuestion q <- f.questions){
  	script += initial(q);
  }

  script += "\n    },
  		    '    methods: {";

  for (AQuestion q <- f.questions) 
    script += checkComputed(q,f);
 
  script += "\n    }
            '})";
      
  return script;
}

str checkComputed(AQuestion q, AForm f) {
  switch(q){
    case computed(str label, str id, AType typ, AExpr expr, src = loc def): 
      return "\n        " + id + ": function() {
    	 	 '            return " + expression2js(expr,f) + ";
			 '        },";
			
    case qlist(list[AQuestion] questions, src = loc def): {
      str new = "";
      for (AQuestion q2 <- questions) new += checkComputed(q2,f);
      return new;
    }
    	
    case ifthenelse(AExpr cond, list[AQuestion] ifqs, list[AQuestion] elseqs, src = loc def): {
      str new = "";
      for (AQuestion q2 <- ifqs) new += checkComputed(q2,f);
  	  for (AQuestion q2 <- elseqs) new += checkComputed(q2,f);
      return new;
    }
    	
    case ifthen(AExpr cond, list[AQuestion] ifqs, src = loc def): {
      str new = "";
      for (AQuestion q2 <- ifqs) new += checkComputed(q2,f);
      return new;
    }
    	
    default: return "";
   
  }
   
  return "";
}

str expression2js(AExpr e, AForm f) {
  switch (e) {
 	case parentheses(AExpr l): 
 	  return "(" + expression2js(l, f) + ")";
 	  
 	case ref(str name): 
 	  return "this." + name;
 	  
 	case integer(int i): 
 	  return toString(i);
 	  
 	case boolean(bool b): 
 	  return toString(b);
 	  
 	case string(str s): 
 	  return "\'" + s[1..-1] + "\'";
 	  
 	case multiplication(AExpr l, AExpr r): 
 	  return expression2js(l, f) + " * " + expression2js(r, f);
 	  
 	case division(AExpr l, AExpr r): 
 	  return expression2js(l, f) + " / " + expression2js(r, f);
 	  
 	case addition(AExpr l, AExpr r): 
 	  return expression2js(l, f) + " + " + expression2js(r, f);
 	  
 	case subtraction(AExpr l, AExpr r): 
 	  return expression2js(l, f) + " - " + expression2js(r, f);
 	  
 	case greater(AExpr l, AExpr r):  
 	  return expression2js(l, f) + " \> " + expression2js(r, f);
 	  
 	case smaller(AExpr l, AExpr r): 
 	  return expression2js(l, f) + " \< " + expression2js(r, f);
 	  
 	case greatereq(AExpr l, AExpr r): 
 	  return expression2js(l, f) + " \>= " + expression2js(r, f);
 	  
 	case smallereq(AExpr l, AExpr r): 
 	  return expression2js(l, f) + " \<= " + expression2js(r, f);
 	  
 	case not(AExpr l): 
 	  return "!" + expression2js(l, f);
 	  
 	case equal(AExpr l, AExpr r): 
 	  return expression2js(l, f) + " == " + expression2js(r, f);
 	  
 	case noteq(AExpr l, AExpr r): 
 	  return expression2js(l, f) + " != " + expression2js(r, f);
 	  
 	case and(AExpr l, AExpr r): 
 	  return expression2js(l, f) + " && " + expression2js(r, f);
 	  
 	case or(AExpr l, AExpr r): 
 	  return expression2js(l, f) + " || " + expression2js(r, f);
  }
  
  return "";
}
