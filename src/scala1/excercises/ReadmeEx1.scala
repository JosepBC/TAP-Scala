package scala1.excercises


object ReadmeEx1 extends scala.App {
  val numbers = 1 to 100
  def even (i:Int) : Boolean = i%2==0

  println("Exercise a")
  println(numbers.filter(_ % 2 != 0).map(i => i * i))

  println("Exercise b")
  def ex2(list:List[Int], f1:Int=>Boolean, f2:Int=>Int):List[Int] = list.filter(f1).map(f2)

  println(ex2(numbers.toList, even, i=>i*i))

  println("Exercise c")
  val evenapply = ex2(_, even, _)

  println(evenapply(numbers.toList, i=>i*i))

  val text = List("hello", "my", "name", "is", "josep")
  println("Exercise d")
  def ex3 [T](list:List[T], f1:T=>Boolean, f2:T=>T):List[T] = list.filter(f1).map(f2)

  println(ex3(text, (x: String) => x.length > 2, (text: String) => text.toUpperCase))
  println(ex3(numbers.toList, even, (i: Int) => i * i))
}
