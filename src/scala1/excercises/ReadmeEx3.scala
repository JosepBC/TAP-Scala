package scala1.excercises

import scala.annotation.tailrec

object ReadmeEx3 {
  def filter[T](cond:T=>Boolean, list: List[T]): List[T] = list match {
    case Nil => Nil
    case x::xs if(cond(x)) => x::filter(cond, xs)
    case _::xs => filter(cond, xs)
  }

  def filter_tail[T](cond:T=>Boolean, list: List[T]): List[T] = {
    @tailrec
    def foo[T](c:T=>Boolean, l: List[T], res: List[T]=Nil): List[T] = l match {
      case Nil => res
      case x::xs if(c(x)) => foo(c, xs, x::res)
      case _::xs => foo(c, xs, res)
    }

    foo(cond, list)
  }

  def reduce_tail[T](operation:(T, T)=> T, list: List[T]): T = {
    @tailrec
    def foo[T](op:(T, T)=> T, l: List[T], res: T): T = l match {
      case Nil => res
      case x::xs => foo(op, xs, op(x, res))
    }
    foo(operation, list.tail, list.head)
  }

  def reduce[T](operation:(T, T)=> T, list: List[T]): T = list match {
    case x::xs if(xs.nonEmpty) => operation(x, reduce(operation, xs))
    case x::_ => x
  }

  def main(args: Array[String]): Unit = {
    println("Filter")
    println(filter[Int](_ > 10, List(10, 20, 30)))
    println(filter_tail[Int](_ > 10, List(10, 20, 30)))

    println("Filter 10")
    val filter10 = filter_tail[Int](_ > 10, _)
    println(filter10(List(10, 20, 30)))

    println("Reduce")
    println(reduce_tail[Int](_ + _, List(10, 20, 30)))
    println(reduce[Int](_ + _, List(10, 20, 30)))

    println("Reduce part parametrized")
    val reduceSumInt = reduce_tail[Int](_ + _, _)
    println(reduceSumInt(List(10, 20, 30)))
  }

}
