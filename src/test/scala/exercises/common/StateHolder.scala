package exercises.common

import exercises.common.Pair._

object states:
  opaque type StateHolder[T] = Pair[List[T]]

  extension [T](io: StateHolder[T])
    def in: List[T]  = io._1
    def out: List[T] = io._2
    def pop: Option[(T, StateHolder[T])] = in.headOption.map((_, (in.tail, out)))
    infix def push(s: T): StateHolder[T] = (in, out.appended(s))
    def unapply: Pair[List[T]] = io

  object StateHolder:
    def apply[T](ss: T*): StateHolder[T] = (ss.toList, List.empty[T])
