class Nfa[A](initialS:A, states:Set[A], finalS:Set[A], transitions:Set[(A, A, Char)], eps_transitions:Set[(A,A)]) {

  // The following methods are only the methods directly called by the test suite. You can (and should) define more.

  def map[B](f: A => B) : Nfa[B] = //??? // TODO implement map
  {
    var new_initialS:B = f(initialS);
    var new_states:Set[B] = states.map(f);
    var new_finalS:Set[B] = finalS.map(f);
    var new_transitions:Set[(B, B, Char)] = Set();
    for(iter <- transitions) {
      var new_trans:(B, B, Char) = (f(iter._1), f(iter._2), iter._3);
      new_transitions = new_transitions + new_trans;
    }
    var new_eps_transitions:Set[(B,B)] = Set();
    for(iter <- eps_transitions) {
      var new_trans:(B, B) = (f(iter._1), f(iter._2));
      new_eps_transitions = new_eps_transitions + new_trans;
    }
    var new_nfa:Nfa[B] = new Nfa(new_initialS, new_states, new_finalS,
      new_transitions, new_eps_transitions);
    return new_nfa;
  }

  def next(state:A, c: Char): Set[A] = //??? // TODO implement next
  {
    var nextStates:Set[A] = Set();

    for(trans <- transitions) {
      if(trans._1 == state) {
        if(trans._3 == c) {
          nextStates = nextStates + trans._2;
        }
      }
    }

    return nextStates;
  }

  def eps_next(state:A): Set[A] = {
    var nextStates:Set[A] = Set();
    for (trans <- eps_transitions) {
      if(trans._1 == state) {
        nextStates = nextStates + trans._2;
      }
    }
    return nextStates;
  }

  def eps_closure(state:A):Set[A] = {
    var closure:Set[A] = Set(state);
    var possible_next:Set[A] = Set();
    possible_next = possible_next ++ eps_next(state);

    while (possible_next.size != 0) {
      var st:A = possible_next.head;
      closure = closure + st;
      possible_next = possible_next.tail;
      possible_next = possible_next ++ eps_next(st);
    }

    return closure;
  }

  def accepts(str: String): Boolean = //??? // TODO implement accepts
  {
    var word:String = str;
    var current_state:Set[A] = Set(initialS);
    var states_to_add:Set[A] = Set();

    while(word.size != 0) {
      for(iter <- current_state) {    
        states_to_add = states_to_add ++ eps_closure(iter);
      }
      current_state = current_state ++ states_to_add;
      states_to_add = Set();

      for(iter <- current_state) {
        states_to_add = states_to_add ++ next(iter, word.head);
      }
      current_state = states_to_add;
      states_to_add = Set();
      word = word.tail;
    }

    states_to_add = Set();
    for(iter <- current_state) {
      states_to_add = states_to_add ++ eps_closure(iter);
    }

    current_state = states_to_add;

    for(iter <- current_state) {
      if(isFinal(iter)) {
        return true;
      }
    }

    return false;
  }

  def getStates : Set[A] =  // TODO implement getStates
  {
    return states;
  }

  def getInitialState : A =
  {
    return initialS;
  }

  def getFinalStates : Set[A] =
  {
    return finalS;
  }

  def getTransitions : Set[(A, A, Char)] = {
    return transitions;
  }

  def getEpsTransitions : Set[(A, A)] = {
    return eps_transitions;
  }

  def isFinal(state: A): Boolean =   // TODO implement isFinal
  {
    for(fin <- finalS) {
      if (state == fin)
        return true;
    }
    return false;
  }

  def getAlphabet : Set[Char] = {
    var alphabet:Set[Char] = Set();
    for (iter <- transitions) {
      alphabet = alphabet + iter._3;
    }
    return alphabet;
  }
}

// This is a companion object to the Nfa class. This allows us to call the method fromPrenex without instantiating the Nfa class beforehand.
// You can think of the methods of this object like static methods of the Nfa class
object Nfa {
  def getUnits(str:String):Array[String] = {
    var units:Array[String] = str.split(" ");
    return units;
  }

  def unitIsOperation(str:String):Boolean = {
    val operations:Array[String] = Array("CONCAT", "STAR", "UNION", "PLUS", "MAYBE");
    return operations.contains(str);
  }

  def unitIsValid(str:String):Boolean = {
    if (unitIsOperation(str)) {return true;}
    if (str == "eps") {return true;}
    if (str.length == 3 && str(0) == '\'') {return true;}
    if(str.length == 1) {return true;}
    return false;
  }

  def unitsAreValid(units:Array[String]):Boolean = {
    var len:Int = units.length - 1;
    for(i <- 0 to len) {
      if(!unitIsValid(units(i))) {
        return false;
      }
    }
    return true;
  }

  def findSpaces(units:Array[String]):Array[String] = {
    var new_units:Array[String] = Array();
    var len:Int = units.length - 1;
    for(i <- 0 to len) {
      if (!units(i).contains("'")) {
        new_units = new_units ++ Array(units(i));
      } else {
        if (i < len && units(i+1) == "'") {
          new_units = new_units ++ Array(" ");
        }
        if(units(i).length == 3 && units(i).head == '\'') {
          var s:String = units(i);
          s = s.tail;
          s = s.head + "";
          new_units = new_units ++ Array(s);
        }
      }
    }

    return new_units;
  }

  def findBiggestState(nfa:Nfa[Int]) : Int = {
    var max_state:Int = 0;
    for (iter <- nfa.getStates) {
      if (iter > max_state) {
        max_state = iter;
      }
    }
    return max_state;
  }

  def addToNfa(offset:Int, nfa:Nfa[Int]):Nfa[Int] = {
    var new_initialS:Int = nfa.getInitialState + offset;
    var new_states:Set[Int] = Set();
    for(iter <- nfa.getStates) {
      new_states = new_states + (iter + offset);
    }

    var new_finalS:Set[Int] = Set();
    for(iter <- nfa.getFinalStates) {
      new_finalS = new_finalS + (iter + offset);
    }

    var new_transitions:Set[(Int, Int, Char)] = Set();
    for(iter <- nfa.getTransitions) {
      var new_trans:(Int, Int, Char) = (iter._1 + offset, iter._2 + offset, iter
        ._3);
      new_transitions = new_transitions + new_trans;
    }

    var new_eps_transitions:Set[(Int,Int)] = Set();
    for(iter <- nfa.getEpsTransitions) {
      var new_trans:(Int, Int) = (iter._1 + offset, iter._2 + offset);
      new_eps_transitions = new_eps_transitions + new_trans;
    }

    var new_nfa:Nfa[Int] = new Nfa(new_initialS, new_states, new_finalS,
      new_transitions, new_eps_transitions);
    return new_nfa;
  }

  def makeEpsNfa:Nfa[Int] = {
    return new Nfa(0, Set(0), Set(0), Set(), Set());
  }

  def makeInvalidNfa:Nfa[Int] = {
    return new Nfa(-1, Set(-1), Set(), Set(), Set());
  }

  def makeCharNfa(c:Char):Nfa[Int] = {
    return new Nfa(0, Set(0,1), Set(1), Set((0,1,c)), Set());
  }


  def concat(nfa1:Nfa[Int], nfa2:Nfa[Int]):Nfa[Int] = {
    var offset:Int = findBiggestState(nfa1) + 1;
    var new_nfa2:Nfa[Int] = addToNfa(offset, nfa2);
    var highest:Int = findBiggestState(new_nfa2);

    var new_intialS:Int = highest + 1;
    var new_finalS:Int = highest + 2;

    var nfa1_states:Set[Int] = nfa1.getStates;
    var nfa2_states:Set[Int] = new_nfa2.getStates;

    var new_states:Set[Int] = nfa1_states  ++ nfa2_states ++ Set(new_intialS, new_finalS);

    var new_trans:Set[(Int,Int,Char)] = nfa1.getTransitions ++
      new_nfa2.getTransitions;

    var new_eps_trans:Set[(Int, Int)] = nfa1.getEpsTransitions ++
      new_nfa2.getEpsTransitions ++ Set((new_intialS, nfa1.getInitialState));

    for(iter <- nfa1.getFinalStates) {
      var tmp:Int = iter;
      var ini2:Int = new_nfa2.getInitialState;
      new_eps_trans = new_eps_trans ++ Set((tmp, ini2));
    }

    for(iter <- new_nfa2.getFinalStates) {
      var tmp:Int = iter;
      new_eps_trans = new_eps_trans ++ Set((tmp, new_finalS));
    }

    return new Nfa(new_intialS, new_states, Set(new_finalS), new_trans,
      new_eps_trans);
  }

  def union(nfa1:Nfa[Int], nfa2:Nfa[Int]):Nfa[Int] = {
    var offset:Int = findBiggestState(nfa1) + 1;
    var new_nfa2:Nfa[Int] = addToNfa(offset, nfa2);
    var highest:Int = findBiggestState(new_nfa2);

    var new_intialS:Int = highest + 1;
    var new_finalS:Int = highest + 2;

    var nfa1_states:Set[Int] = nfa1.getStates;
    var nfa2_states:Set[Int] = new_nfa2.getStates;

    var new_states:Set[Int] = nfa1_states  ++ nfa2_states ++ Set(new_intialS, new_finalS);

    var new_trans:Set[(Int,Int,Char)] = nfa1.getTransitions ++
      new_nfa2.getTransitions;

    var new_eps_trans:Set[(Int, Int)] = nfa1.getEpsTransitions ++
      new_nfa2.getEpsTransitions ++ Set((new_intialS, nfa1.getInitialState),
      (new_intialS, new_nfa2.getInitialState));

    for(iter <- nfa1.getFinalStates) {
      var tmp:Int = iter;
      new_eps_trans = new_eps_trans ++ Set((tmp, new_finalS));
    }

    for(iter <- new_nfa2.getFinalStates) {
      var tmp:Int = iter;
      new_eps_trans = new_eps_trans ++ Set((tmp, new_finalS));
    }

    return new Nfa(new_intialS, new_states, Set(new_finalS), new_trans,
      new_eps_trans);
  }

  def star(nfa1:Nfa[Int]):Nfa[Int] = {
    var highest:Int = findBiggestState(nfa1);

    var new_intialS:Int = highest + 1;
    var new_finalS:Int = highest + 2;

    var nfa1_states:Set[Int] = nfa1.getStates;

    var new_states:Set[Int] = nfa1_states  ++ Set(new_intialS, new_finalS);

    var new_trans:Set[(Int,Int,Char)] = nfa1.getTransitions;

    var new_eps_trans:Set[(Int, Int)] = nfa1.getEpsTransitions ++
      Set((new_intialS, nfa1.getInitialState),(new_intialS, new_finalS));

    for(iter <- nfa1.getFinalStates) {
      var tmp:Int = iter;
      new_eps_trans = new_eps_trans ++ Set((tmp, new_finalS), (tmp,
        nfa1.getInitialState));
    }

    return new Nfa(new_intialS, new_states, Set(new_finalS), new_trans,
      new_eps_trans);
  }

  def plus(nfa1:Nfa[Int]):Nfa[Int] = {
    var nfa2:Nfa[Int] = star(nfa1);
    var nfa:Nfa[Int] = concat(nfa1, nfa2);
    return nfa;
  }

  def maybe(nfa1:Nfa[Int]):Nfa[Int] = {
    var nfa:Nfa[Int] = union(nfa1, makeEpsNfa);
    return nfa;
  }

  def indexJumpOver(units:Array[String], current_pos:Int): Int = {
    var ok:Int = 0;
    var pos:Int = current_pos;
    var last:Int = units.length - 1;

    if(units(pos + 1).length == 1) {
      return pos + 2;
    }

    if (units(pos + 1) == "CONCAT" || units(pos + 1) == "UNION"){
      ok = 2;
    } else if (units(pos+1) == "STAR" || units(pos+1) == "PLUS" || units(pos+1) == "MAYBE") {
      ok = 1;
    }

    pos = pos + 1;

    while(pos != last) {
      pos = pos + 1;

      if(units(pos) == "CONCAT" || units(pos) == "UNION") {
        ok = ok + 1;
      } else if (units(pos) != "STAR" && units(pos) != "PLUS" && units(pos) != "MAYBE") {
        ok = ok - 1;
      }

      if(ok == 0) {
        return pos + 1;
      }
    }
    return -1;
  }

  def fromUnits(units:Array[String], current_pos:Int): Nfa[Int] = {
    if (!unitsAreValid(units)) {
      return makeInvalidNfa;
    }

    var uni:Array[String] = findSpaces(units);

    if(uni(current_pos) == "eps") {
      return makeEpsNfa;
    } else if (uni(current_pos).length == 1) {
      return makeCharNfa(uni(current_pos).head);
    } else if(uni(current_pos) == "CONCAT") {
      var next_pos:Int = indexJumpOver(uni, current_pos);
      return concat(fromUnits(uni, current_pos + 1), fromUnits(uni, next_pos));
    } else if(uni(current_pos) == "UNION") {
      var next_pos:Int = indexJumpOver(uni, current_pos);
      return union(fromUnits(uni, current_pos + 1), fromUnits(uni,
        next_pos));
    } else if(uni(current_pos) == "STAR") {
      return star(fromUnits(uni, current_pos + 1));
    } else if(uni(current_pos) == "PLUS") {
      return plus(fromUnits(uni, current_pos + 1));
    } else if(uni(current_pos) == "MAYBE") {
      return maybe(fromUnits(uni, current_pos + 1));
    }

    return makeEpsNfa;
  }

  def fromPrenex(str: String): Nfa[Int] = // TODO implement Prenex -> Nfa transformation.
  {
    var units:Array[String] = getUnits(str);
    var len:Int = units.length - 1;

    var nfa:Nfa[Int] = fromUnits(units, 0);
    return nfa;
  }

  // You can add more methods to this object
}