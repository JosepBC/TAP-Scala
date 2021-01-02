package scala1.excercises

import scala.annotation.tailrec

object Recursion {

  def lower3(list: List[Int]) : Int = list match {
    case Nil => 0
    case x :: xs => lower3(xs) + (if (x < 3) x else 0)
  }

  @tailrec
  def higher(list: List[Int], highest:Int) : Int = list match {
    case Nil => highest
    case x :: xs => if(x > highest) higher(xs, x) else higher(xs, highest)
  }

  @tailrec
  def listIntegers(n:Int, list: List[Int]):List[Int] = n match {
    case 0 => list
    case _ => listIntegers(n - 1, list.appended(n))
  }

  def list_between(begin: Int, end: Int, listX: List[Int]): List[Int] = {
    @tailrec
    def list_inner(start: Int, En: Int, originalList: List[Int], list: List[Int]): List[Int] = start match {
      case En => list
      case _ => list_inner(start+1, En, originalList, list.appended(listX(start)))
    }

    list_inner(begin-1, end, listX, Nil)
  }

  def list_between_even(begin: Int, end: Int, listX: List[Int]): List[Int] = {
    @tailrec
    def list_inner_even(start: Int, En: Int, originalList: List[Int], list: List[Int]): List[Int] = start match {
      case En => list
      case _ => if(listX(start) % 2 != 0) list_inner_even(start+1, En, originalList, list) else list_inner_even(start+1, En, originalList, list.appended(listX(start)))
    }

    list_inner_even(begin-1, end, listX, Nil)
  }

  def digits(num: Int): Int = math.log10(num).toInt

  def first(number: Int): Int = number / math.pow(10, digits(number)).toInt

  def zeroNumbers(ammount: Int):Int = math.pow(10, ammount).toInt

  def tail_digit(digit: Int): List[Int] = {

    @tailrec
    def tail_inner_digit(dig: Int, list: List[Int]): List[Int] = dig match {
      case 0 => list
      case _ => tail_inner_digit(dig - (first(dig) * zeroNumbers(digits(dig))), list.appended(first(dig)))
    }

    tail_inner_digit(digit, Nil)
  }

  def stack_digit(digit: Int): List[Int] = digit match {
        case 0 => Nil
        case _ => first(digit)::stack_digit(digit - (first(digit) * zeroNumbers(digits(digit))))
  }


  def factorize(x: Int): List[Int] = {
    def foo(x: Int, a: Int): List[Int] = x % a match {
      case _  => if(a * a > x) List(x) else foo(x, a + 1)
      case 0 => a::foo(x / a, a)
    }
    foo(x, 2)
  }

  def inverse_center(str: String): String = {
    if(str.length<=1) return str
    def foo(str2: String, it: Int, str3: String): String= it match {
      case 0 => str3
      case _ => foo(str2, it - 1, str3.concat(str.charAt(it).toString))
    }

    str.charAt(0).toString++foo(str, str.length - 2, "")++str.charAt(str.length-1).toString
  }

  def power(a: Int, b:Int):Int = {
    @tailrec
    def foo(a1: Int, b1:Int, res:Int): Int = b1 match {
      case 0 => res
      case _ => foo(a1, b1-1, res*a1)
    }
    foo(a, b, 1)
  }

  def stackPower(a: Int, b:Int):Int = b match {
    case 0 => 1
    case _ => a*stackPower(a, b-1)
  }

  def main(args: Array[String]): Unit = {
    val numbers = 1 to 10
    println("Add numbers lower than 3 in list: "+numbers)
    println(lower3(numbers.toList))

    println("Higher value in list: "+numbers)
    val higherCall = higher(_, Int.MinValue)
    println(higherCall(numbers.toList))

    println("List of integers from 0 to 10")
    val listCall = listIntegers(_, Nil)
    println(listCall(10))

    println("List between elem 1 and 4 of: "+numbers)
    println(list_between(1, 4, numbers.toList))

    println("Elems between elem 1 and 4 and even of list: "+numbers)
    println(list_between_even(1, 4, numbers.toList))

    println("Tail digit of 3645")
    println(tail_digit(3645))

    println("Stack digit of 3645")
    println(stack_digit(3645))


    println("Factorize")
    println(factorize(8))

    println("Reverse center")
    println(inverse_center("exam"))

    println("Power")
    println(power(3, 4))

    println("Stack power")
    println(stackPower(3, 4))
  }

}

