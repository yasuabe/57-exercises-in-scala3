package exercises.common

import exercises.common.Pair._

object states:
  opaque type StateHolder[T, U] = (List[T], List[U])

  extension [T, U](io: StateHolder[T, U])
    def in: List[T]  = io._1
    def out: List[U] = io._2
    def pop: Option[(T, StateHolder[T, U])] = in.headOption.map((_, (in.tail, out)))
    infix def push(s: U): StateHolder[T, U] = (in, out.appended(s))
    def unapply: (List[T], List[U]) = io

  object StateHolder:
    def apply[T, U](ss: T*): StateHolder[T, U] = (ss.toList, List.empty[U])
