package exercises

import scala.annotation.tailrec

trait MySet[A] extends (A => Boolean) {

  /*
    EXERCISE - implement a functional set
   */
  def apply(elem: A): Boolean = contains(elem)

  def contains(elem: A): Boolean

  def +(elem: A): MySet[A]

  def ++(anotherSet: MySet[A]): MySet[A]

  def map[B](f: A => B): MySet[B]

  def flatMap[B](f: A => MySet[B]): MySet[B]

  def filter(predicate: A => Boolean): MySet[A]

  def foreach(f: A => Unit): Unit

  /*
    EXERCISE #2
    - removing and element
    - intersection with another set
    - difference with another set
   */
  def -(elem: A): MySet[A]

  def --(anotherSet: MySet[A]): MySet[A] // difference

  def &(anotherSet: MySet[A]): MySet[A] // intersection

}

class EmptySet[A] extends MySet[A] {
  override def contains(elem: A): Boolean = false

  override def +(elem: A): MySet[A] = new NonEmptySet[A](elem, this)

  override def ++(anotherSet: MySet[A]): MySet[A] = anotherSet

  override def map[B](f: A => B): MySet[B] = new EmptySet[B]

  override def flatMap[B](f: A => MySet[B]): MySet[B] = new EmptySet[B]

  override def filter(predicate: A => Boolean): MySet[A] = this

  override def foreach(f: A => Unit): Unit = ()

  override def -(elem: A): MySet[A] = this

  override def --(anotherSet: MySet[A]): MySet[A] = this

  override def &(anotherSet: MySet[A]): MySet[A] = this

}

class NonEmptySet[A](head: A, tail: MySet[A]) extends MySet[A] {
  override def contains(elem: A): Boolean = elem == head || tail.contains(elem)

  override def +(elem: A): MySet[A] =
    if (this contains elem) this
    else new NonEmptySet[A](elem, this)

  /*
    [1 2 3] ++ [4 5] =
    [2 3] ++ [4 5] + 1 =
    [3] ++ [4 5] + 1 + 2 =
    [] ++ [4 5] + 1 + 2 + 3 =
    [4 5] + 1 + 2 + 3 = [4 5 1 2 3]
  */
  override def ++(anotherSet: MySet[A]): MySet[A] = tail ++ anotherSet + head

  /*
    [1 2 3]map (x + 1) =
    [2 3] map (x + 1) + 1+1 =
    [3] map (x + 1) + 2 + 2+1 =
    [] map (x + 1) + 2 + 3 + 3+1 = [2 3 4]
   */
  override def map[B](f: A => B): MySet[B] = (tail map f) + f(head)

  override def flatMap[B](f: A => MySet[B]): MySet[B] = (tail flatMap f) ++ f(head)

  override def filter(predicate: A => Boolean): MySet[A] = {
    val filteredTail = tail filter predicate
    if (predicate(head)) filteredTail + head
    else filteredTail
  }

  override def foreach(f: A => Unit): Unit = {
    f(head)
    tail foreach f
  }

  override def -(elem: A): MySet[A] =
    if (head == elem) tail
    else tail - elem + head

  override def --(anotherSet: MySet[A]): MySet[A] = filter(x => !anotherSet(x))

  override def &(anotherSet: MySet[A]): MySet[A] = filter(anotherSet)
}

object MySet {
  def apply[T](values: T*): MySet[T] = {
    @tailrec
    def buildSet(valSeq: Seq[T], acc: MySet[T]): MySet[T] =
      if (valSeq.isEmpty) acc
      else buildSet(valSeq.tail, acc + valSeq.head)

    buildSet(values, new EmptySet[T])
  }
}

object MySetPlayground extends App {
  val s = MySet(1, 2, 3, 4)
  s foreach println
}