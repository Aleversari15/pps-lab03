package u03

import u03.Sequences
import u03.Sequences.Sequence.*
import u03.Sequences.*
import u02.Modules.Person
import u02.Modules.Person.Teacher
import u02.Modules.*

import scala.annotation.tailrec

object Lab03:

  object Lab03:
    //TASK 2
    def findCourses(p: Sequence[Person]): Sequence[String] =
    flatMap(filter(p)(!isStudent(_))) (t => t match
        case Teacher(n,c) => Cons(c,Nil()))

    def foldLeft[A](s: Sequence[A])(elem: A)(op: (A,A) => A): A =
      @tailrec
      def _loop(s: Sequence[A], acc: A ): A = s match
        case Cons(h,t) => _loop(t,op(acc,h))
        case _ => acc
      _loop(s,elem)

    def countCourses(s: Sequence[Person]): Int =
      @tailrec
      def _loop(seq: Sequence[String], acc: Int): Int = seq match
        case Cons(h,t) => _loop(t, acc + 1)
        case Nil() => acc
      _loop(findCourses(s), 0)

    //TASK 3