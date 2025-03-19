package u03

import u03.Sequences.Sequence.*
import u03.Sequences.*
import u02.Modules.Person.Teacher
import u02.Modules.*
import scala.annotation.tailrec

object Lab03:

  object Lab03:
    //TASK 1
    def skip[A](s: Sequence[A])(n: Int): Sequence[A] = (s, n) match
      case (Nil(), _) => Nil()
      case (Cons(_, t), n) if n != 0 => skip(t)(n - 1)
      case (Cons(h, t), _) => Cons(h, t)

    def zip[A, B](first: Sequence[A], second: Sequence[B]): Sequence[(A, B)] = (first, second) match
      case (Nil(), _) => Nil()
      case (_, Nil()) => Nil()
      case (Cons(h1, t1), Cons(h2, t2)) => Cons((h1, h2), zip(t1, t2))

    def concat[A](s1: Sequence[A], s2: Sequence[A]): Sequence[A] = (s1, s2) match
      case (Nil(), _) => s2
      case (_, Nil()) => s1
      case (Cons(h, t), Cons(h2, t2)) => Cons(h, concat(t, Cons(h2, t2)))

    def reverse[A](s: Sequence[A]): Sequence[A] =
      @tailrec
      def _loop(seq: Sequence[A], acc: Sequence[A]): Sequence[A] = seq match
        case Nil() => acc
        case Cons(h, t) => _loop(t, Cons(h, acc))
      _loop(s, Nil())

    def flatMap[A, B](s: Sequence[A])(mapper: A => Sequence[B]): Sequence[B] =
      @tailrec
      def _loop(seq: Sequence[A], acc: Sequence[B]): Sequence[B] = seq match
        case Nil() => acc
        case Cons(h, t) => _loop(t, concat(acc, mapper(h)))
      _loop(s, Nil())


    //TASK 2
    def findCourses(p: Sequence[Person]): Sequence[String] =
    flatMap(Sequence.filter(p)(!isStudent(_))) (t => t match
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

    //TASK 3 (In Streams.scala)

