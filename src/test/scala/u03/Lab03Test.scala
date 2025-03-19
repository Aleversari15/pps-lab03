package u03

import org.junit.Test
import org.junit.*
import org.junit.Assert.*
import u03.Sequences
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



