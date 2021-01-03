package scala1.excercises

object ReadmeEx5 {

  def main(args: Array[String]): Unit = {
    val persons = List(Person("Josep", 1234567890, 20), Person("Bello", 1234567890, 10), Person("Bello", 1234567890, 50))
    println("Oldest person")
    println(persons.reduce((p1: Person, p2: Person) => if (p1.age > p2.age) p1 else p2))
    println("Phones of persons whose age is greater than 40")
    println(persons.filter(_.age > 40))
  }
}
