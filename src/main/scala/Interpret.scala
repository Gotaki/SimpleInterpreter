/*
 * This file is part of COMP3000 assignment 1.
 *
 * Copyright (C) 2021 Kym Haines, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

// https://www.scala-lang.org/api/2.12.8/scala/util/matching/Regex.html
// https://www.scala-lang.org/api/2.12.8/scala/collection/immutable/List.html
// https://www.scala-lang.org/api/2.12.8/scala/collection/immutable/StringLike.html
// https://www.scala-lang.org/api/2.12.8/scala/collection/immutable/Map.html

package org.mq.interpret

import scala.util.matching._

object Interpret {

  sealed abstract class LObject
  case class LSymbol(sym:String) extends LObject
  case class LNumber(num:Int) extends LObject
  case class LList(head:LObject, tail:LObject) extends LObject

  val nil = LSymbol("nil")
  val T = LSymbol("t")
  val error = LSymbol("ERROR")
  var a = LSymbol("A")


  def resetEnv:Unit =
  {
    // case LSymbol(_) => LSymbol
    
    // TO DO: reset your functions/variables environment
  }

  val pat = "[()]|[0-9|a-z\\+-/*]+".r //regex for all
  // "TO DO: recognise all the language tokens".r
  val patnum = "-?[0-9]+".r //find all numbers including negative integers

  val patname = "[a-zA-Z][a-zA-Z0-9]*".r //find name

 
  def matchPat(line:String):List[String] =
                              pat.findAllIn(line.toLowerCase).toList

  // excludes brackets
  def strToLObj(s:String):LObject =
    {
      s match {
        case patnum() => LNumber(s.toInt)
        case patname() => LSymbol(s)
        case pat() => LSymbol(s)
        
        }
    }
    // TO DO: convert a string token to an LObject; or error


  def tokensToLObjs(a:List[String]):Option[LObject] = 
    {
      a match  {
        case patname() => Option(strToLObj(a(0)))
        
    
      } 
  }  


  //   // TO DO: convert a list of token strings to an LObject
  //   // NOTE: anywhere () is seen, it should be interpreted as nil
  //   None
  // }

  // for testing
  def lineToLObj(line:String):LObject = tokensToLObjs(matchPat(line)) match
  {
  case Some(s) => s
  case None    => error
  }

  //  def valuemap(varName:String, value:LObject):Map[String,LObject] = {
  //   Map("nil" => nil) 
  //  }

  var mapval : Map[String,LObject] = Map("value" -> LNumber('0'))

  def setValue(varName:String, value:LObject):Unit = (varName,value) match {
    case (varName,LSymbol(value)) => mapval = mapval + (varName -> LSymbol(value))
    case (varName,LNumber(value)) => mapval = mapval + (varName -> LNumber(value))
    case (varName,LList(head,tail)) => mapval = mapval + (varName -> LList(head,tail))
}
  

  def getValue(varName:String):LObject = {
     mapval(varName)
   }

    //val getValue : Map[String,LObject] = Map("varname" -> value)
    // (varName) match {
    // TO DO: get the value of a variable; or error if variable not defined
    //  case LSymbol("varName") => varName
    //  case _      => error
  // }

  def add(a:LObject, b:LObject):LObject = (a,b) match {
    case (LNumber(a), LNumber(b)) => LNumber (a+b)
    case _ => error
  }
      

  def sub(a:LObject, b:LObject):LObject = (a,b) match {
    case (LNumber(a), LNumber(b)) => LNumber (a-b)
    case _ => error
  }

  def mul(a:LObject, b:LObject):LObject = (a,b) match {
    case (LNumber(a), LNumber(b)) => LNumber (a*b)
    case _ => error
  }

  def div(a:LObject, b:LObject):LObject = (a,b) match {
    case (LNumber(a), LNumber(b)) => LNumber (a/b)
    case _ => error
  }

  def car(a:LObject):LObject = 
  {
    a match {
      case LList(head, _) => head
      case _ => error
    }
  }
  def cdr(a:LObject):LObject = 
  {
   a match {
     case LList(_, tail) => tail
     case _ => error
    } 
  }

  def cons(a:LObject, b:LObject):LObject = (a,b) match {
    case (LSymbol(a),LList(b,c)) => LList(LSymbol(a),LList(b,c))
    case _ => error
  }
    

  def eeqq(a:LObject, b:LObject):LObject = (a==b) match {
    case true => T
    case _ => nil
  }
  
  def setq(v:String, b:LObject):LObject =  {
    setValue(v,b)
    getValue(v)
  }
  
  
  //error    // TO DO

  def iiff(cond:LObject, ifThen:LObject, ifElse:LObject):LObject = (cond,ifThen,ifElse) match{
    case tru if (cond == T) => eval(ifThen)
    case _ => eval(ifElse)
  } 

  def defun(name:String, arg:String, body:LObject):LObject =
  {
    // case LSymbol (name) = 

    // TO DO: define a function
    // the function definition source would look like:
    //      (def name (arg) body)
   LSymbol(name) 
  }

  def funCall(name:String, arg:LObject):LObject = error    // TO DO

  def eval(a: LObject): LObject = a match {
    case LSymbol("nil") => nil
    case LSymbol("t")   => T
    // TO DO: add cases for all the other possibilities in the eval table
    //        in the spec
    case LNumber(a) => LNumber(a) 
    case LList(head, tail) => head match {
   //Perform Functions Here
      case LSymbol("quote") => tail match {
        case LList(head, tail) => head
        case _ => error
      }
      case LSymbol("car") => tail match {
        case LList(head, tail) => car (eval(head))
        case _ => error
      }
      case LSymbol("cdr") => tail match {
        case LList(head, tail) => cdr (eval(head))
        case _ => error
      }
      case LSymbol("cons") => tail match {
        case LList(head, tail) => tail match {
          case LList(hd, tl) => cons(eval(head), eval(hd))
          case _  => error
        }
         case _   => error
      }
      case LSymbol("eval") => tail match {
         case LList(head, tail) => head match {
          case LList(head2, tail2) => tail2 match {
            case LList(head3, tail3) => eval(head3)
            case _ => error
          }
          case _ => error
         }
        case _ => error
      }
      case LSymbol("eq") =>  tail match {
        case LList(head, tail) => tail match {
          case LList(head2, tail2) => eeqq(eval(head), eval(head2))
        }
        case _ => error
      }
       case LSymbol("if") =>  tail match {
        case LList(head, tail) => tail match {
            case LList(head2, tail2) => iiff(eval(head), eval(head2), nil)
            case _ => error
          }
          case _ => error
      }
      case LSymbol("+") => tail match{
        case LList(head, tail) => tail match{
          case LList(head2, tail2) => add(head, head2)
          case _ => error
        }
      }
      case LSymbol("-") => tail match{
        case LList(head, tail) => tail match{
           case LList(head2, tail2) => sub(eval(head), eval(head2))
            case _ => error
        }
    }
      case LSymbol("*") => tail match{
        case LList(head, tail) => tail match{
           case LList(head2, tail2) => mul(eval(head), eval(head2))
            case _ => error
        }
    } 
    case LSymbol("/") => tail match{
        case LList(head, tail) => tail match{
           case LList(head2, tail2) => div(eval(head), eval(head2))
            case _ => error
        }
    } 
    case _ => error
    }
    case _ => error
  }


  def showLine(s:LObject):Unit = { show(s);  println() }

  def show(s:LObject):Unit = s match
  {
  case LList(h, t)  => print("(")
                       show(h)
                       showList(t)
                       print(")")
  case LSymbol(a)   => print(a)
  case LNumber(a)   => print(a)
  }

  def showList(s:LObject):Unit = s match
  {
  case LSymbol("nil") =>
  case a:LList => print(" ")
                  show(a.head)
                  a.tail match
                  {
                  case b:LList => showList(b)
                  case _ =>
                  }
  case _ => print(" . ")
            show(s)
  }

}
