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

void compile(AForm f) {
  VEnv venv = initialEnv(f);
  writeFile(f.src[extension="js"].top, form2js(f,venv));
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
                 p([ question2html(q) | q <- f.questions ])
               )
             )
           )
         );
}

HTML5Node question2html(AQuestion q){
return html();
}

str form2js(AForm f, VEnv venv) {
 str script = "var vue = new Vue({
 			  '       el: \'#vue\',
 			  '     data: {";
 			  
 set[str] JSquestions = {};
 
 for(str name <- venv){
   JSquestions += name;
   script += "\n   " + id + ": " + name.\value;
 }

  script += "\n    },
  		  ' 	methods: {";

 for(AQuestion q <- f){
  switch(q){
    case computed(str label, str id, AType typ, AExpr expr, src = loc def): {
    	JSquestions += id;
    	script += "\n    " + id + ": function() {
    	 	'            return " + "INSERT FUNCTION HERE" + ";
			'        },";
    }
    default: continue;
   }
 }
 
   script += "\n    }
          '})";
      
  return script;
}
