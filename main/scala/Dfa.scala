import scala.collection.mutable
import scala.collection.mutable.Queue

class Dfa[A] (initialS:A, states:Set[A], finalS:Set[A], transitions:Set[(A, A, Char)]){

  // The following methods are only the methods directly called by the test suite. You can (and should) define more.

  def map[B](f: A => B) : Dfa[B] =  // TODO implement map
  {
    var new_initialS:B = f(initialS);
    var new_states:Set[B] = states.map(f);
    var new_finalS:Set[B] = finalS.map(f);
    var new_transitions:Set[(B, B, Char)] = Set();
    for(iter <- transitions) {
      var new_trans:(B, B, Char) = (f(iter._1), f(iter._2), iter._3);
      new_transitions = new_transitions + new_trans;
    }

    var new_dfa:Dfa[B] = new Dfa(new_initialS, new_states, new_finalS,
      new_transitions);
    return new_dfa;
  }

  def next(state:A, c: Char): A =  // TODO implement next
  {
    var nextState:A = initialS;

    for(trans <- transitions) {
      if(trans._1 == state) {
        if(trans._3 == c) {
          nextState = trans._2;
        }
      }
    }
    return nextState;
  }

  def accepts(str: String): Boolean = // TODO implement accepts
  {
    var word:String = str;
    var current_state:A = initialS;
    var next_state:A = initialS;

    while(word.length != 0) {
      next_state = next(current_state, word.head);
      if(next_state == initialS) {
        var ok: Int = 1;
        for(tr <- transitions) {
          if(tr._1 == current_state && tr._2==next_state && tr._3 == word.head) {
            ok = 0;
          }
        }
        if(ok == 1) {
          return false;
        }
      }
      current_state = next_state;
      word = word.tail;
    }

    if(isFinal(current_state)) {
      return true;
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

  def isFinal(state: A): Boolean = // TODO implement isFinal
  {
    for(fin <- finalS) {
      if (state == fin)
        return true;
    }
    return false;
  }


}

// This is a companion object to the Dfa class. This allows us to call the method fromPrenex without instantiating the Dfa class beforehand.
// You can think of the methods of this object like static methods of the Dfa class
object Dfa {
  def getSetPos(s:Set[Set[Int]], v:Set[Int]): Int = {
    var pos:Int = 0;
    for(iter <- s) {
      if(iter == v) {
        return pos;
      }
      pos = pos + 1;
    }
    return -1;
  }

  def nfaToDfa(nfa:Nfa[Int]):Dfa[Int] = {
    var set_states:Set[Set[Int]] = Set();
    var set_initial:Set[Int] = nfa.eps_closure(nfa.getInitialState);
    set_states = set_states + set_initial;
    var set_final:Set[Set[Int]] = Set.empty;
    var set_trans:Set[(Set[Int], Set[Int], Char)] = Set.empty;
    var alphabet:Set[Char] = nfa.getAlphabet;

    val q:mutable.Queue[Set[Int]] = mutable.Queue(set_initial);
    while(q.nonEmpty) {
      var crt_state:Set[Int] = q.dequeue();
      set_states = set_states + crt_state;
        for(ch <- alphabet) {
          var new_set:Set[Int] = Set.empty;
          var new_trans:Set[(Set[Int], Set[Int], Char)] = Set.empty;

          for (st <- crt_state) {
            for (trans <- nfa.getTransitions) {
              if(trans._1 == st && trans._3 == ch) {
                new_set = new_set + trans._2;
              }
            }
          }
          var to_add:Set[Int] = Set.empty;
          for(st <- new_set) {
            to_add = to_add ++ nfa.eps_closure(st);
          }
          new_set = to_add;
          if(new_set.nonEmpty) {
            new_trans = Set((crt_state, new_set, ch));
            set_trans = set_trans ++ new_trans;
          }
          if(new_set.nonEmpty && !set_states.contains(new_set)) {
            q.enqueue(new_set);
          }
        }
    }

    for(fin <- nfa.getFinalStates) {
      for(st <- set_states) {
        if(st.contains(fin)) {
          set_final = set_final + st;
        }
      }
    }

    var dfa_init:Int = getSetPos(set_states, set_initial);
    var dfa_state:Set[Int] = Set.empty;
    var dfa_finals:Set[Int] = Set.empty;
    var dfa_trans:Set[(Int, Int, Char)] = Set();

    for(i <- 0 until set_states.size) {
      dfa_state = dfa_state + i;
    }

    for(st <- set_final) {
      dfa_finals = dfa_finals + getSetPos(set_states, st);
    }

    for(trans <- set_trans) {
      var first:Int = getSetPos(set_states, trans._1);
      var second:Int = getSetPos(set_states, trans._2);
      dfa_trans = dfa_trans + ((first, second, trans._3));
    }

    return new Dfa(dfa_init, dfa_state, dfa_finals, dfa_trans);
  }

  def fromPrenex(str: String): Dfa[Int] =  // TODO implement Prenex -> Dfa transformation. hint: you should make use of Nfa.fromPrenex to build the Dfa
  {
    var corresponding_nfa:Nfa[Int] = Nfa.fromPrenex(str);
    var dfa:Dfa[Int] = nfaToDfa(corresponding_nfa);

    return dfa;
  }

  // You can add more methods to this object
}
