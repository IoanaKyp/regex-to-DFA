import java.util.regex._
import scala.collection.mutable

object Regex {
  /*
    This function should:
    -> Classify input as either character(or string) or operator
    -> Convert special inputs like [0-9] to their correct form
    -> Convert escaped characters
  */

  def transformSugar(chars:List[Char]):List[Char] = {
    var s = chars.mkString
    var canCopy:Int = 0;
    var res:String = "";
    var pos:Int = 0;
    for (i <- 0 until s.length) {
      if(s(i) == '[' && i + 4 < s.length && s(i+2) == '-' && s(i+4) == ']'){
        var start = s(i+1);
        var end = s(i+3);
        val interval = "(" ++ (start to end).map(_.toChar).mkString("|") ++ ")";
        canCopy = 4;
        res = res ++ interval;
      } else {
        if(canCopy == 0) {
          res = res + s(i)
        } else {
          canCopy = canCopy - 1;
        }
      }
    }
    return res.toList;
  }

  def addConcat(chars:List[Either[Char,Char]]): List[Either[Char,Char]] = {
    var result:List[Either[Char,Char]] = List();
    var len:Int = chars.length;

    for(i <- 0 until len) {
      if(i != len - 1) {
        result = result ++ List(chars(i));
        if(chars(i).isRight) {
          if(chars(i+1).isRight) {
            result = result ++ List(Left('.'));
          }
          if(chars(i+1) == Left('(')) {
            result = result ++ List(Left('.'));
          }
        } else {
          if (chars(i) == Left('*') || chars(i) == Left('+') || chars(i) == Left('?')) {
            if(chars(i+1).isRight || chars(i+1) == Left('(')) {
              result = result ++ List(Left('.'));
            }
          }
          if(chars(i) == Left(')')) {
            if(chars(i+1) == Left('(') || chars(i+1).isRight) {
              result = result ++ List(Left('.'));
            }
          }
        }
      } else {
        result = result ++ List(chars(i));
      }
    }

    return result;
  }

  def getOpPriority(op:Char):Int = {
    op match {
      case '(' => return 5
      case ')' => return 1
      case '*' => return 4
      case '?' => return 4
      case '+' => return 4
      case '.' => return 3
      case '|' => return 2
    }
    return 0;
  }

  def getOpCode(op:Char) : String = {
    op match {
      case '*' => return "STAR"
      case '?' => return "MAYBE"
      case '+' => return "PLUS"
      case '.' => return "CONCAT"
      case '|' => return "UNION"
      case _ => return ""
    }
    return "";
  }

  def preprocess(s:List[Char]): List[Either[Char,Char]] = {
    var result:List[Either[Char,Char]] = List();
    var aux:List[Char] = transformSugar(s.toList);
    val normalChars = ('0' to '9').map(_.toChar) ++ ('a' to 'z').map(_.toChar) ++ ('A' to 'Z').map(_.toChar) ++ "ε";
    val opChar = "+" ++ "?" ++ "|" ++ "(" ++ ")" ++ "*";
    var canWrite:Int = 0;

    var len:Int = aux.length;
    for(i <- 0 until len) {
      if (aux(i)=='\'') {
        if(i + 2 < len && aux(i+2)=='\'') {
          val ch:Char = aux(i+1);
          result = result ++ List(Right(ch));

          canWrite = 2;
        }else {
          canWrite = canWrite - 1;
        }
      } else if (canWrite == 0) {
        if (normalChars.contains(aux(i))) {
            result = result ++ List(Right(aux(i)));
        } else {
          result = result ++ List(Left(aux(i)));
        }
      }  else {
        canWrite = canWrite - 1;
      }
    }


    return addConcat(result);
    //return result;
  }

  // This function should construct a prenex expression out of a normal one.
  def toPrenex(str: String): String = {

    val eps_creator = "eps".r;
    val shrink = eps_creator.replaceAllIn(str, "ε");

    var res:List[Char] = transformSugar(shrink.toList);
    var result:List[Either[Char, Char]] = preprocess(res);
    var ret:List[String] = List();
    var prenex:String = "";

    val s:mutable.Stack[Char] = mutable.Stack();
    var rev_tokens:List[Either[Char, Char]] = result.reverse;

    for(tok <- rev_tokens) {
      tok match {
        case Right(value) => {
          if (value == ' ') {
            ret = ret ++ List("'");
            ret = ret ++ List(value.toString);
            ret = ret ++ List("'");
            ret = ret ++ List(" ");
          } else if (value == 'ε'){
            ret = ret ++ List("eps");
            ret = ret ++ List(" ");
          } else {
            ret = ret ++ List(value.toString);
            ret = ret ++ List(" ");
          }

        }
        case Left(value) => {
          if(value == '(') {
            while (s.top != ')') {
              var c:Char = s.pop();
              ret = ret ++ List(getOpCode(c));
              ret = ret ++ List(" ");
            }
            s.pop();
          } else if (value == ')') {
            s.push(value);
          } else {
            while (s.nonEmpty && getOpPriority(s.top) > getOpPriority(value)) {
              ret = ret ++ List(getOpCode(s.pop()));
              ret = ret ++ List(" ");
            }
            s.push(value);
          }
        }
      }

    }
    while (s.nonEmpty) {
      ret = ret ++ List(getOpCode(s.pop()));
      ret = ret ++ List(" ");
    }

    ret = ret.reverse;
    for(t <- ret) {
      prenex = prenex ++ t;
    }

    return prenex.tail;
  }
}



