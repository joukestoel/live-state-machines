module NxstTranslator

import StateMachine;
import Parser;
import lang::nextstep::Syntax;
import lang::nextstep::InstanceSyntax;
import lang::nextstep::OutputSyntax;
import Pipeline;

import List;
import Map;
import Set;
import IO;
import String;
import ParseTree;

alias RuntimeModel = tuple[str currentState, map[str,int] visitCount];

bool canParseSpec(str spec) {
  try {
    parseString("<spec> input { old static old runtime new static }");
    return true;
  } catch ParseError(loc err): {
    return false;
  } 
}

RuntimeModel getNextRuntimeModel(str nxstpSpec, Controller oldCtl, Controller newCtl, RuntimeModel currentRuntime) {
  Spec spc = sm2nxstpModel(nxstpSpec);
  NextepInstance inst = sm2nxstpInst(oldCtl, newCtl, currentRuntime);
  OutputDef rawNewRuntime = runAndGetNextModel(spc, inst[@\loc = spc@\loc]);
  
  println(rawNewRuntime);
  RuntimeModel newRuntime = nxstp2smModel(rawNewRuntime);
  println(newRuntime);
  
  return newRuntime;
}

NextepInstance sm2nxstpInst(Controller oldCtl, Controller newCtl, RuntimeModel currentRuntime) 
  = parseInstanceString(sm2nxstpInstance(oldCtl, newCtl, currentRuntime));

RuntimeModel nxstp2smModel(OutputDef rawModel) {
  str getCurrentState() = "<state>" when ObjectDef obj <- rawModel.newRuntime, "<obj.\type>" == "Runtime", /(FieldInstantiation)`current = <Atom state>` := obj.fields;
  default str getCurrentState() { throw "No current state found"; }
  
  str currentState = getCurrentState();  
  
  map[str,int] visitCount = ();
  for (ObjectDef obj <- rawModel.newRuntime, "<obj.\type>" == "Visit") {
    if (/(FieldInstantiation)`state = <Atom state>` := obj.fields, /(FieldInstantiation)`nr = <Int nr>` := obj.fields) {
      visitCount["<state>"] = toInt("<nr>");
    }
  }
  
  return <currentState, visitCount>;
}

@memo
Spec sm2nxstpModel(nxstpSpec) = parseString(nxstpSpec);

str nxstpStaticModel() =
  "static {
  '  class Machine {      
  '    states: State*
  '    initial: State
  '   
  '    invariants {
  '      // initial must be a known state
  '      initial in states
  '      // all states that are targeted in the transitions must be part of the machine
  '      states.transitions.target in states
  '    }
  '  }
  '
  '  class State {
  '    transitions: Trans*
  '  }
  '
  '  class Trans  {
  '    target: State
  '  }
  '}
  '";

str nxstpRuntimeModel() =
  "runtime {
  '  class Runtime  {
  '    machine: Machine
  '    current: State
  '    visited: Visit*
  '    
  '    invariants {
  '      // the current state must be part of the states connected to the machine
  '      current in machine.states
  '
  '      forall s:machine.states | one (visited.state & s)
  '      forall v1:visited, v2:visited | v1 != v2 =\> v1.state != v2.state
  '    }
  '  } 
  '
  '  class Visit {
  '    state: State
  '    nr: int
  '    
  '    // Nr must be positive
  '    invariant: nr \>= 0
  '  }
  '}
  '";
  
str nxstpMigrationModel() 
  = "migration {
    '  // if the old current state does not exist anymore in the new machine states then the new current state is the machines initial state
    '  not old[Runtime.current] in new[Runtime.machine.states] =\> (new[Runtime.current] = new[Runtime.machine.initial])
    '}";

str sm2nxstpInstance(Controller oldCtl, Controller newCtl, RuntimeModel currentRuntime) 
  = "input {
    '  old static
    '    <sm2nxstpParseStatic(oldCtl)>
    '
    '  old runtime
    '    <sm2nxstpParseRuntime(currentRuntime)>
    '
    '  new static
    '    <sm2nxstpParseStatic(newCtl)> 
    '}";

str sm2nxstpParseStatic(Controller ctl) {
  str getInitialState() =  "<s.name>" when StateMachine::State s <- ctl.states, /Initial i := s;
  default str getInitialState() { throw "No initial state found"; }

  str getStates() = intercalate(", ", ["<s.name>" | State s <- ctl.states]) when /State _ := ctl.states;
  default str getState() = "[]";
  
  str genTransitions(State s) = intercalate(",", ["<s.name>_<t.event>" | Transition t <- s.transitions]) when /Transition _ := s.transitions;
  default str genTransitions(State s) = "[]";
 
  return "Machine doors
         '  states = <getStates()>
         '  initial = <getInitialState()>
         '<for (State s <- ctl.states) {>
         'State <s.name>
         '  transitions = <genTransitions(s)>
         '<}>
         '<for (State s <- ctl.states, Transition t <- s.transitions) {>
         'Trans <s.name>_<t.event>
         '  target = <t.state>
         '<}>";
}

str sm2nxstpParseRuntime(RuntimeModel currentRuntime) {
  list[str] orderedKeys = toList(currentRuntime.visitCount<0>);
  
  return "Runtime x
         '  machine = doors
         '  current = <currentRuntime.  currentState>
         '  visited = <intercalate(", ", ["v<i>" | int i <- [0..size(orderedKeys)]])> 
         '
         '<for (int i <- [0..size(orderedKeys)]) {>
         'Visit v<i>
         '  state = <orderedKeys[i]>
         '  nr = <currentRuntime.visitCount[orderedKeys[i]]>
         '<}>
         '";
}