package scala1.excercises

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer


object Exercises2 {
  def filter[T](f:(T => Boolean), list: List[T]): List[T] = list match {
    case Nil => Nil
    case x::xs => if(f(x)) x::filter(f, xs) else filter(f, xs)
  }

  def zipWith[T](f:(T,T)=>T, list1: List[T], list2: List[T]):List[T] = (list1, list2) match {
    case (Nil, Nil) => Nil
    case (x::xs, t::ts) => f(x, t)::zipWith(f, xs, ts)
  }

  def divide[T](list: List[T], f:T => Boolean):(ListBuffer[T], ListBuffer[T]) = {
    @tailrec
    def div[T](l: List[T], f:T => Boolean, go: ListBuffer[T], noGo: ListBuffer[T]):(ListBuffer[T], ListBuffer[T]) = l match {
      case Nil => (go, noGo)
      case x::xs => if(f(x)) div(xs, f, go+=x, noGo) else div(xs, f, go, noGo+=x)
    }
    div(list, f, ListBuffer(), ListBuffer())
  }

  @tailrec
  def foldr_filter[T](f:T => Boolean, op: (T, T) => T, l: List[T], res: T): T = l match {
    case Nil => res
    case x::xs => if(f(x)) foldr_filter(f, op, xs, op(x, res)) else foldr_filter(f, op, xs, res)
  }

  def count_car(char: Char, list: List[String]): Int = {
    //list.mkString("").count(_ == char)
    list.foldLeft("")((z: String, f: String) => z.concat(f))
      .filter((a :Char)=> a.equals(char))
      .map(_=>1)
      .fold(0)(_+_)
  }

  def remove_primes(n: Int, list: List[Int]) : Int = {
    n - list.filter((x: Int) => Recursion.factorize(x).length == 1).fold(0)(_+_)
  }

  def invest_word(text: List[String]): List[String] = {
    text.map((a: String) => Recursion.inverse_center(a))
  }


  def operate_if(op:(Int, Int) => Int, initOp: Int,condition:(Int, Int) => Boolean, cmpNumber: Int, list: List[String]) = {
    list
      .map(str => Integer.parseInt(str, 2))
      .filter((p=>condition(p, cmpNumber)))
      .fold(initOp)(op)
  }

  def main(args: Array[String]): Unit = {
    val list = List(1,7,2,9,67,3)
    println("Filter < 3 in "+list)
    println(filter((x: Int) => x < 3, list))

    println("zipwith")
    println(zipWith((x: Int, y: Int) => x + y, List(1, 2, 3), List(4, 5, 6)))
    println(zipWith((a: String, b: String) => a.concat(b), List("hello", "world"), List("my", "name")))

    println("Divide")
    val numbers = 1 to 5
    println(divide(numbers.toList, (x: Int) => x < 3))

    println("Folder filter")
    println(foldr_filter[Int]((x: Int) => x > 4, (a: Int, b: Int) => a + b, List(3, 6, 4, 5), 0))

    println("Count e")
    val stringList = List("el","examen","esta","especialmente","escogido","entre","los","mas","elementales")
    println(count_car('e', stringList))

    println("Remove primes")
    println(remove_primes(100, List(2, 3, 4, 5, 6, 7, 8, 9, 10)))

    println("Invest word")
    println(invest_word(List("dicen","que","si","tenemos","un","texto","con","las","palabras","separadas","y","cambiamos","el","orden","interno","manteniendo","la","primera","y","ultima","letra","podemos","ser","capaces","de","leerlo","entendiendo","el","mensaje")))

    println("Operate if")
    println(operate_if(_+_, 0, (a, b) => a > b, 3, List("10", "11", "111", "1110")))
    println(operate_if(_*_, 1, (x, y) => x % y == 0, 2, List("10", "11", "111", "1110")))

    println("APPLIED PARTIALLY FUNCTIONS")
    println("Power 3")
    val power3 = Recursion.power(_, 3)
    println("3^3="+power3(3))

    println("Add pairs")
    val add_pairs = foldr_filter[Int](x => x % 2 == 0, _+_, _, 0)
    println(add_pairs(List(2, 4, 5, 6)))

    println("Put zeros")
    val put_zeros = Recursion.power(10, _)
    println(put_zeros(4))

    println("Less 100 primes")
    val less_hundred_primes = remove_primes(100, _)
    println(less_hundred_primes(List(2, 3, 4, 5, 6)))
  }
}
