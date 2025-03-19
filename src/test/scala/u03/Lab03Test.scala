package u03

import org.junit.Test
import org.junit.*
import org.junit.Assert.*
import u03.Sequences
import u03.Streams.*
import u03.Streams.Stream.*
import u03.Sequences.Sequence.*
import u03.Sequences.*
import u02.Modules.*
import u02.Modules.Person.{Student, Teacher}

class Lab03Test:
  import u03.Lab03.Lab03.*

  val p1 = Teacher("Mario", "Storia")
  val p2 = Student("Alessandra", 1)
  val p3 = Teacher("Anna", "Matematica")
  val seq: Sequence[Person] = Cons(p1, Cons(p2, Cons(p3, Nil())))

  //TASK 2 - LISTS
  @Test def testFindCourses() =
    val coursesExpected: Sequence[String] = Cons("Storia", Cons("Matematica", Nil()))
    assertEquals(coursesExpected, findCourses(seq))
    assertEquals(Nil(), findCourses(Nil()))

  @Test def testFoldLeft() =
    val lst = Cons(3, Cons(7, Cons(1, Cons(5, Nil()))))
    assertEquals(17, foldLeft(lst)(1)(_ + _))
    assertEquals(-16, foldLeft(lst)(0)(_ - _))

  @Test def testCountCourses() =
    assertEquals(2, countCourses(seq))

  //TASK 3 - STREAMS
  @Test def takeWhile(): Unit =
    val str1 = Stream.iterate(0)(_ + 1) // {0,1,2,3,..}
    val str2 = Stream.takeWhile(str1)(_ < 5) // {0,1,2,3,4}
    assertEquals(Cons(0, Cons(1, Cons(2, Cons(3, Cons(4, Nil()))))), Stream.toList(str2))

  @Test def testFill(): Unit =
    val seq: Sequence[String] = Cons("a", Cons("a", Cons("a", Nil())))
    assertEquals(seq, Stream.toList(fill(3)("a")))

  @Test def testFibonacci(): Unit =
    assertEquals(Cons(0, Cons(1, Cons(2, Cons(3, Cons(5, Nil()))))), Stream.toList(Stream.take(fibonacci)(5)))



