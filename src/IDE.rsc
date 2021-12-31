@license{
  Copyright (c) Tijs van der Storm <Centrum Wiskunde & Informatica>.
  All rights reserved.
  This file is licensed under the BSD 2-Clause License, which accompanies this project
  and is available under https://opensource.org/licenses/BSD-2-Clause.
}
@contributor{Tijs van der Storm - storm@cwi.nl - CWI}

module IDE

import salix::App;
import salix::HTML;
import salix::Node;
import salix::Core;
import salix::lib::CodeMirror;
import salix::lib::Mode;
import salix::lib::XTerm;
import salix::lib::Mode;
import salix::lib::UML;
import salix::lib::Dagre;

import util::Maybe;
import ParseTree;
import String;
import List;
import IO;

import StateMachine;
import NxstTranslator;
//import lang::nextstep::Syntax;

SalixApp[IDEModel] liveSMApp(str id = "live-state-machines") = makeApp(id, ideInit, ideView, ideUpdate, parser = parseMsg);

App[IDEModel] liveSMWebApp() 
  = webApp(liveSMApp(), |project://live-state-machines/src/index.html|, |project://live-state-machines/src|); 

alias IDEModel = tuple[
  str src, 
  str nxstpModelSrc,
  Maybe[start[Controller]] lastParse,
  Maybe[str] currentState,
  str currentCommand,
  Mode mode, // put it here, so not regenerated at every click..
  //Mode nxstMode,
  map[str, int] visitCount
];
  
Maybe[start[Controller]] maybeParse(str src) {
  try {
    return just(parse(#start[Controller], src));
  }
  catch ParseError(loc err): {
    return nothing();
  }
}  
  
IDEModel ideInit() {
  Mode stmMode = grammar2mode("statemachine", #Controller);
  //Mode nxstMode = grammar2mode("nxstp", #Spec);
  
  IDEModel model = <"", "", nothing(), nothing(), "", stmMode, ()>;
  
  model.src = doors();
  model.lastParse = maybeParse(model.src);
  
  model.nxstpModelSrc = nxstpStaticModel() + nxstpRuntimeModel() + nxstpMigrationModel();
  
  if (just(start[Controller] ctl) := model.lastParse) {
    model.visitCount = ("<s.name>" : 0 | StateMachine::State s <- ctl.top.states);
  
    if (StateMachine::State s <- ctl.top.states, /Initial i := s) {
      model.currentState = just("<s.name>");
    }
  }  
 
  return model;
}

IDEModel reset(IDEModel model) {
  if (just(start[Controller] ctl) := model.lastParse) {
    model.visitCount = ("<s.name>" : 0 | StateMachine::State s <- ctl.top.states);
  
    if (StateMachine::State s <- ctl.top.states, /Initial i := s) {
      model.currentState = just("<s.name>");
    }
  }  
  
  return model;  
}

str ctl2plantuml(start[Controller] ctl, Maybe[str] currentState, map[str,int] visitCount) {
  list[str] states = [ "<s.name>" | StateMachine::State s <- ctl.top.states];
  
  list[str] trans = [ "<s.name> --\> <t.state> : <t.event>" |
     StateMachine::State s <- ctl.top.states,
     Transition t <- s.transitions ];
     
  bool isActive(str s) = s == cur
    when just(str cur) := currentState;
  
  return 
    "@startuml
    'scale 1.2
    'skinparam state {
    '  BackgroundColor\<\<Current\>\> Pink
    '}
    '
    '<intercalate("\n", [ "state \"<s>\" as <s> <isActive(s) ? "\<\<Current\>\>" : "">" | s <- states])>
    '<intercalate("\n", [ "<s> : visits = <"<s>" in visitCount ? visitCount["<s>"] : 0>" | s <- states])> 
    '[*] -\> <states[0]>
    '<intercalate("\n", trans)>
    '@enduml";
}

list[str] stmComplete(IDEModel model, str prefix) {
  list[str] cs = [];
  if (just(start[Controller] ctl) := model.lastParse) {
    if (/<word:[a-zA-Z0-9_]+>$/ := prefix) {
      for (StateMachine::State s <- ctl.top.states, startsWith("<s.name>", word)) {
        cs += ["<prefix[0..size(prefix) - size(word)]><s.name>"];
      }
      for (/StateMachine::Event e := ctl, startsWith("<e.name>", word)) {
        cs += ["<prefix[0..size(prefix) - size(word)]><e.name>"];
      }
    }
  }
  return cs + [prefix];
}

Maybe[str] stmHighlight(str x) {
  if (/goto <rest:.*>/ := x) {
    return just("\u001B[1;35mgoto\u001B[0m <rest>");
  }
  if (/event <rest:.*>/ := x) {
    return just("\u001B[1;35mevent\u001B[0m <rest>");
  }
  else {
    return nothing();
  }
}

str doors() = 
    "state closed initial
    '  open =\> opened
    'end
    '
    'state opened
    '  close =\> closed
    'end";

data Msg
  = stmChange(int fromLine, int fromCol, int toLine, int toCol, str text, str removed)
  | nxstpModelChange(int fromLine, int fromCol, int toLine, int toCol, str text, str removed)
  | fireEvent(str name)
  | gotoState(str name)
  | noOp()
  ;

tuple[Msg, str] myEval(str command) {
  if (/event <event:.*>/ := command) {
    return <fireEvent(event), "ok">;
  }
  if (/goto <state:.*>/ := command) {
    return <gotoState(state), "ok">;
  }
  return <noOp(), "Not a command \"<command>\", try \"event \<eventName\>\", or \"goto \<stateName\>\"">;
}


Maybe[str] transition(str currentState, str event, start[Controller] ctl) {
  println("transition: <currentState> (<event>)");
  
  if (StateMachine::State s <- ctl.top.states, "<s.name>" == currentState) { 
    if (Transition t <- s.transitions, "<t.event>" == event) {
      return just("<t.state>");
    }
  }
  
  return nothing();
} 

IDEModel ideUpdate(Msg msg, IDEModel model) {

  list[str] myComplete(str prefix) = stmComplete(model, prefix);
  
  void doTransition(str event) {
    println("do trans <event>");
    if (just(start[Controller] ctl) := model.lastParse) {
       println("ctl <ctl>");
      if (just(str current) := model.currentState) {
         println("cur <current>");
        Maybe[str] state = transition(current, event, ctl);
        
        if (just(str nextState) := state) {
          model.visitCount[nextState] += 1;
          model.currentState = just(nextState);
        }
      }
    }
  }
  
  switch (msg) {
  
    case stmChange(int fromLine, int fromCol, int toLine, int toCol, str text, str removed): { 
      model.src = updateSrc(model.src, fromLine, fromCol, toLine, toCol, text, removed);
      
      if (just(start[Controller] ctl) := maybeParse(model.src)) {
        
        if (just(start[Controller] prevCtl) := model.lastParse, just(str currentState) := model.currentState) {
          RuntimeModel newRuntime = getNextRuntimeModel(model.nxstpModelSrc, prevCtl.top, ctl.top, <currentState, model.visitCount>);

          model.currentState = just(newRuntime.currentState);
          model.visitCount = newRuntime.visitCount;
        }
        
        model.lastParse = just(ctl);
      }  
    }
    
    case nxstpModelChange(int fromLine, int fromCol, int toLine, int toCol, str text, str removed): { 
      model.nxstpModelSrc = updateSrc(model.nxstpModelSrc, fromLine, fromCol, toLine, toCol, text, removed);
      
      if (canParseSpec(model.nxstpModelSrc), just(start[Controller] ctl) := maybeParse(model.src)) {
        
        if (just(start[Controller] prevCtl) := model.lastParse, just(str currentState) := model.currentState) {
          RuntimeModel newRuntime = getNextRuntimeModel(model.nxstpModelSrc, prevCtl.top, ctl.top, <currentState, model.visitCount>);

          model.currentState = just(newRuntime.currentState);
          model.visitCount = newRuntime.visitCount;
        }
        
        model.lastParse = just(ctl);
      }  
    }    
          
    case fireEvent(str event): 
      doTransition(event);
  }
  
  return model;
}

list[str] mySplit(str sep, str s) {
  if (/^<before:.*?><sep>/m := s) {
    return [before] + mySplit(sep, s[size(before) + size(sep)..]);
  }
  return [s];
}

str updateSrc(str src, int fromLine, int fromCol, int toLine, int toCol, str text, str removed) {
  list[str] lines = mySplit("\n", src);
  int from = ( 0 | it + size(l) + 1 | str l <- lines[..fromLine] ) + fromCol;
  int to = from + size(removed);
  str newSrc = src[..from] + text + src[to..];
  return newSrc;  
}

Maybe[str] initialState(start[Controller] ctl) {
  if (StateMachine::State s <- ctl.top.states, /Initial i := s) {
	  return just("<s.name>");
  }
  return nothing();
}
 
Maybe[StateMachine::State] lookupCurrentState(start[Controller] ctl, IDEModel model) {
  if (StateMachine::State s <- ctl.top.states, isCurrentState(s, model)) {
    return just(s);
  }
  return nothing();
}
 
bool staleCurrentState(start[Controller] ctl, IDEModel model) 
  = !any(StateMachine::State s <- ctl.top.states, isCurrentState(s, model));
 
bool isCurrentState(StateMachine::State s, IDEModel model)
  = just(str current) := model.currentState && current == "<s.name>";

void ideView(IDEModel model) {
  div(() {
    div(class("row"), () {
      div(class("col-md-12"), () {
	      h3("Live Modeling with Nextep demo");
	    });
    });
    
    div(class("row"), () {
      div(class("col-md-8"), () {
        div(class("panel panel-default"), () {
          div(class("panel-heading"), () {
            h3(class("panel-title"), () { span("Model and Meta-model"); });
          });

          div(class("panel-body"), () {
            ul(class("nav nav-tabs"), () {
              li(class("active"), () { 
                a(href("#model"), attr("data-toggle","tab"), () { span("Runtime model"); }); 
              });
              li(() { 
                a(href("#meta-model"), attr("data-toggle","tab"), () { span("Meta-model"); }); 
              });
            });
            
            div(class("tab-content"), () {
              div(attr("role","tab-pane"), class("tab-pane active"),id("model"), () { 
                codeMirrorWithMode("myCodeMirror", model.mode, onChange(stmChange), height(500), 
                  mode("statemachine"), indentWithTabs(false), lineNumbers(true), \value(model.src));
              });
              
              div(attr("role","tab-pane"), class("tab-pane"),id("meta-model"), () { 
                codeMirror("myCodeMirror2", model.mode, onChange(nxstpModelChange), height(500), 
                  indentWithTabs(false), lineNumbers(true), \value(model.nxstpModelSrc));
              });
            });
          });
        });
        
      });
    
      div(class("col-md-4"), () {
        div(class("panel panel-default"), () {
          div(class("panel-heading"), () {
            h3(class("panel-title"), () { span("Interpreter"); });
          });

          div(class("panel-body"), () {
            if (just(start[Controller] ctl) := model.lastParse) {
              div(uml2svgNode(ctl2plantuml(ctl, model.currentState, model.visitCount)));

              div(() {
                h5("Possible transitions:");
                ul(class("list-inline list-unstyled"), () {
                  for (StateMachine::State s <- ctl.top.states, isCurrentState(s, model), Transition t <- s.transitions) {
                    li(class("list-inline-item"), () { button(class("btn btn-primary"), onClick(fireEvent("<t.event>")), "<t.event> =\> <t.state>"); });
                 }
                });
             });
            }
          });
        });  
      });
    });
  });
}

