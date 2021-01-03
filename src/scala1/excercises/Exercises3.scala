package scala1.excercises

object Exercises3 {

  def apply_if(cond:(Int) => Boolean, operation:(Int) => Int, list: List[Number]):List[Number] = list match {
    case Nil => Nil
    case x::xs if(cond(x.num)) =>  Number(operation(x.num), operation(x.num).toHexString)::apply_if(cond, operation, xs)
    case _::xs => apply_if(cond, operation, xs)
  }

  def apply(cond:(Int) => Boolean, operation:(Int) => Int, list: List[Number]): List[Number] = {
    apply_if(cond, operation,list.filter(n=> n.num.toHexString.equals(n.hex)))
  }

  def approved_list(students: List[Student]): List[String] = {
    students
      .filter(stu=> ((stu.marks.fold(0.0)(_+_)) / stu.marks.length) > 5)
      .filter(stu=> stu.marks.forall(_>=4))
      .map(stu=> stu.name + " | ")
  }

  def main(args: Array[String]): Unit = {
    println("Apply if")
    println(apply_if(_ > 15, _ * 2, List(Number(10,"a"), Number(20,"14"), Number(30,"1e"))))

    println("Apply")
    println(apply(_ > 15, _ * 2, List(Number(10, "a"), Number(20, "14"), Number(30, "fffff"))))

    println("Square")
    val square = apply((a: Int) => a==a, (a:Int)=> a*a, _)
    println(square(List(Number(2, "2"), Number(3, "fffff"), Number(4, "4"), Number(5, "5"))))

    println("Approved list")
    println(approved_list(List(Student("Gandalf", List(9, 10, 9.5)), Student("Frodo", List(6, 9, 4)), Student("Sam", List(8, 3, 9)), Student("Merry", List(4, 7, 4.5)), Student("Pippin", List(4.5, 5, 4)))))

  }

}
