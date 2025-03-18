package u03

import u03.Sequences
import u03.Sequences.Sequence.*
import u03.Sequences.*
import u02.Modules.Person
import u02.Modules.Person.Teacher
import u02.Modules.*

object Lab03:

  object Lab03:
    def findCourses(p: Sequence[Person]): Sequence[String] =
    flatMap(filter(p)(!isStudent(_))) (t => t match
        case Teacher(n,c) => Cons(c,Nil()))

