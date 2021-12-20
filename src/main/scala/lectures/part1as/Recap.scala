package lectures.part1as

import scala.annotation.tailrec

object Recap extends App {

  val aCondition: Boolean = false
  val aConditionedVal = if (aCondition) 42 else 65
  //instruction vs expressions

  //compiler infers types for us
  val aCodeBlock = {
    if (aCondition) 54
    56
  }

  // Unit = void
  val theUnit = println("hello, Scala")

  //functions
  def aFunction(x: Int): Int = x + 1

  //recursion: stack and tail
  @tailrec def factoriel(n: Int, accumulator: Int): Int =
    if (n <= 0) accumulator
    else factoriel(n - 1, n * accumulator)

  //object-oriented programming
  class Animal

  class Dog extends Animal

  val aDog: Animal = new Dog //subtyping polymorphism

  trait Carnivore {
    def eat(a: Animal): Unit
  }

  class Crocodile extends Animal with Carnivore {
    override def eat(a: Animal): Unit = println("crunch!")
  }

  //generics
  abstract class MyList[+A]

  //singletons and companions
  object MyList

  //case classes
  case class Person(name: String, age: Int)

  //exceptions and trt/catch/finally

  val throwsException = throw new RuntimeException //Nothing
  val aPotentialFailure = try {
    throw new RuntimeException
  } catch {
    case e: RuntimeException => "I caught and exception"
  } finally {
    println("some logs")
  }

  //packaging and imports

  //functionak programming
  val incrementer = new Function[Int, Int] {
    override def apply(v1: Int): Int = v1 + 1
  }

  incrementer(1)

  val anonymousIncrementer = (x: Int) => x + 1
  List(1, 2, 3).map(anonymousIncrementer)
  //map, flatMap, filter

  //for-comprehension
  val pairs = for {
    num <- List(1, 2, 3)
    char <- List('1', '2', '3')
  } yield num + "-" + char

  //Scala colections: Seqs, Arrays, Lists, Vectors, Maps, Tuples
  val aMap = Map(
    "Daniel" -> 789,
    "Jess" -> 555
  )

  //"collections":Options, Try
  val anOption = Some(2)

  //pattern matching
  val x = 2
  val order = x match {
    case 1 => "first"
    case 2 => "second"
    case 3 => "third"
    case _ => x + "th"
  }

  val bob = Person("Bob", 22)
  val greeting = bob match {
    case Person(n, _) => s"Hi, my name is $n"
  }


}
