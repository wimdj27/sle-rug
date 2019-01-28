module Compile

import AST;
import Resolve;
import Eval;
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
    case regular(str l, str i, AType typ, src = loc d):
      switch (typ) {
        case string(): return p(label(l), input(\type("text"),  id(i), vmodel(i)), vif(condition));
        
        case integer(): return p(label(l), input(\type("number"),  id(i), vmodel(i)), vif(condition));
        
        case boolean(): return p(label(l), input(\type("checkbox"), id(i), vmodel(i)), vif(condition));
      }
      
    case computed(str l, str i, AType typ, AExpr expr, src = loc d):
      return p(label(l), "{{ <i>() }}", vif(condition));
      
    case qlist(list[AQuestion] questions, src = loc d):
      for (AQuestion question <- questions) question2html(question, f, "true");
      
    case ifthenelse(AExpr expr, list[AQuestion] ifqs, list[AQuestion] elseqs, src = loc d): 
      return div(
        div(
          [ question2html(question, f, expression2js(expr, f)) | AQuestion question <- ifqs ]
        ),
        div(
          [ question2html(question, f, ("!(" + expression2js(expr, f)) + ")") | AQuestion question <- elseqs ]
        )
      );
    
    case ifthen(AExpr expr, list[AQuestion] ifqs): 
      return div(
        [ question2html(question, f, expression2js(expr, f)) | AQuestion question <- ifqs ]
      );
            
    default: return p();
  }
}

str valueToString(Value v){
  switch(v){
    case vint(int x): return toString(x);
    case vbool(bool bl): return toString(bl);
    case vstr(str s): return "\""+s+"\"";
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
    //return "\n        " + id + ": " + (typ==integer() ? "0" : (typ==boolean() ? "false" : "\'\'")) + ",";
    
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
        for (AQuestion q <- ifqs) new += initial(q2);
        return new;
    }
    
    default: return "";
  }
}


str form2js(AForm f) {
  str script = "var form = new Vue({
 			   '    el: \'#form\',
 			   '    data: {";
 			  
  set[str] JSquestions = {};
 
  for(AQuestion q <- f.questions){
    //JSquestions += q.id;
  	script += initial(q);
  }

  script += "\n    },
  		    '    methods: {";

  for(AQuestion q <- f.questions) 
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
  switch(e){
 	case parentheses(AExpr a): 
 	  return "(" + expression2js(a,f) + ")";
 	  
 	case ref(str name): 
 	  return name;
 	  
 	case integer(int i): 
 	  return toString(i);
 	  
 	case boolean(bool bl): 
 	  return toString(bl);
 	  
 	case string(str s): 
 	  return s;
 	  
 	case multiplication(AExpr a, AExpr b): 
 	  return expression2js(a,f) + " * " + expression2js(b,f);
 	  
 	case division(AExpr a, AExpr b): 
 	  return expression2js(a,f) + " / " + expression2js(b,f);
 	  
 	case addition(AExpr a, AExpr b): 
 	  return expression2js(a,f) + " + " + expression2js(b,f);
 	  
 	case subtraction(AExpr a, AExpr b): 
 	  return expression2js(a,f) + " - " + expression2js(b,f);
 	  
 	case greater(AExpr a, AExpr b): 
 	  return expression2js(a,f) + " \> " + expression2js(b,f);
 	  
 	case smaller(AExpr a, AExpr b): 
 	  return expression2js(a,f) + " \< " + expression2js(b,f);
 	  
 	case greatereq(AExpr a, AExpr b): 
 	  return expression2js(a,f) + " \>= " + expression2js(b,f);
 	  
 	case smallereq(AExpr a, AExpr b): 
 	  return expression2js(a,f) + " \<= " + expression2js(b,f);
 	  
 	case not(AExpr a): 
 	  return "!" + expression2js(a,f);
 	  
 	case equal(AExpr a, AExpr b): 
 	  return expression2js(a,f) + " === " + expression2js(b,f);
 	  
 	case noteq(AExpr a, AExpr b): 
 	  return expression2js(a,f) + " != " + expression2js(b,f);
 	  
 	case and(AExpr a, AExpr b): 
 	  return expression2js(a,f) + " && " + expression2js(b,f);
 	  
 	case or(AExpr a, AExpr b): 
 	  return expression2js(a,f) + " || " + expression2js(b,f);
  }
  
  return "";
}
