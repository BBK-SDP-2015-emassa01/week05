package thursday

sealed trait IntList {

  def sum (total:Int=0): Int = 
    this match {
    case EndOfList => total
    case Pair(head, tail) =>tail.sum(head + total)
  }
  
  def product (total:Int=1): Int = 
    this match {
    case EndOfList => total
    case Pair(head, tail) =>tail.product(head * total)
  }
  
    def abstraction(end:Int, f: (Int,Int) => Int):Int = 
      this match {
      case EndOfList=> end
      case Pair(head, tail) => f(head, tail.abstraction(end, f))
    }
    
    /*
     * abstraction(0,lam) => f(1,  p..2,3,4,E.abstraction(0, lam))
     * p..2,3,4,E.abstraction(0, lam => evaluates to f(2, p..3,4,E.abstraction(0, lam))
     * p..3,4,E.abstraction(0, lam)=> evaluates to f(3, p..4,5,E.abstraction(0,lam))
     * p..4,5,E.abstraction(0,lam)=> eval f(4, p..5,E.abstraction(0,lam))
     * p..5,E.abstraction(0,lam) => evaluates to f(E.abstraction(0,lam))
     * E.abstraction(0,lam) eval to => end which is 0
     * 
     * Then unwind.
     * so, return 
     * f(x,y) => x+y
     * 0+4+3+2+1 = 10!
     * 
     * 
     * abstraction can be changed to 'fold' 
     */
    
      def sum:Int = 
      abstraction(0, (head, tail) => head + tail)
      
      def length:Int = 
      abstraction(0, (head, tail) => 1 + tail)
      
      def product :Int =
      abstraction(1, (head, tail) => head * tail)
}
//case classes are simply for the boiler plate code.
final case class Pair(head:Int, tail:IntList) extends IntList
//object is only one
final case object EndOfList extends IntList

//method object

object Main extends App //to save boiler plate, no other reason
{
def list = Pair(1, Pair(2, Pair(3, Pair(4,EndOfList))))
println("list: " +list)
println("sum: " +sum(list))
println("product: " +product(list))
println("length: " +length(list))
println("double: " +double(list))

println("abstract: " +list.sum)

//recurive algebraic data type pattern

import scala.annotation.tailrec

//def sum (List:IntList): Int = 
//    List match {
//    case EndOfList => 0
//    case Pair(head, tail) => head + sum(tail)
//  }
//how can we change it to be tail recurive? IF we eliminate 'head + sum(tail)' as this is the part
//that is not tail recursive
@tailrec
def sum (List:IntList, total:Int=0): Int = 
    List match {
    case EndOfList => total
    case Pair(head, tail) =>sum(tail, head + total)
}
@tailrec
def product (List:IntList, total:Int=1): Int = 
    List match {
    case EndOfList => total
    case Pair(head, tail) =>product(tail, head * total)
}

@tailrec
def length (List:IntList, total:Int=0): Int = 
    List match {
    case EndOfList => total
    case Pair(head, tail) =>length(tail, 1 + total)
}


/*is this tail recursive? Not, because each time it adds the head to it. 
 * Each sum is a new stack, we have to hold the stack frame to work through this.
 * We may run out of stack space, the heap and the stack tend to grow together
 * How can we get rid of this, we want it to be tail recurive, so we dont have to stack the stack frames
 * and dont have to come back.The JVM does not automatically do tail recursion, you have to program it
 * SCALA  has a lot of type checking with it, so you can add an annotation if you use @tailrec
 */

//=====================
//@tailrec //MAY BE ON EXAM!!
def double(List:IntList): IntList =
  List match {
  case EndOfList => EndOfList
  case Pair(head, tail) => Pair(head*2, double(tail))
}
//this is not tail recursive!

//FUNCTION LITERALS
/*
 * up vote
31
down vote
accepted
A function literal is an alternate syntax for defining a function. It's useful for when you 
want to pass a function as an argument to a method (especially a higher-order one like a fold
 or a filter operation) but you don't want to define a separate function. Function literals 
 are anonymous -- they don't have a name by default, but you can give them a name by binding
  them to a variable. A function literal is defined like so:

(a:Int, b:Int) => a + b
You can bind them to variables:

val add = (a:Int, b:Int) => a + b
add(1, 2) // Result is 3
Like I said before, function literals are useful for passing as arguments to higher-order 
functions. They're also useful for defining one-liners or helper functions nested within 
other functions.
 * 
 */

def square(x:Int):Int = x * x
//takes an argument, theres a computation, and returns something.
//lets get rid of square and write this appropriately.

//function object. lambda expression
(x:Int)=> x * x

//I could assign it to something
val f = (x:Int)=> x * x

println (f(3))

val g = (x:Int, y:Int)=> x * y

val x = () => "ahfjdhf" //takes nothing!!
  
  
  
  //LETS ABSTRACT OVER THE METHODS USING THE FUNCTION LITERALS!
//thats above.
  
  
  val s = Seq(1,2,3)
  s.map(x=>x)
  s.map((x)=>x)
  s.map(x=>x*x)
  s.map(x=>x*2)
  //there is also flatmap

  val t = Seq(Seq(1,2,3),Seq(4,5,6))
  t.map(x=>x) //works
 // t.map(x=>x*x) //wont work because you can't list*list!
  
  val z = Seq(1,2,3)
  val y = Seq(4,5,6)
  
  z.zipWithIndex
  z zip y
  
  println("ZIPMAP: " + (z zip y).map(z=>y) )// look it up in the API
  //ZIPMAP: List(List(4, 5, 6), List(4, 5, 6), List(4, 5, 6))
  
  
  /*sometimes you want to return multiple things from a functional literal, how do we do this?
   * in java we wrap up objects in a object.
   * We want to return a tuple, we should be able to in SCALA!
   */
  
//  val d = Tuple(1.2.3.4)
  



}