package exercises.common

import exercises.common.Pair._

object InOut:
  opaque type InOut = Pair[List[String]]

  extension (io: InOut)
    def in: List[String]  = io._1
    def out: List[String] = io._2
    def pop: Option[(String, InOut)] = in.headOption.map((_, (in.tail, out)))
    infix def push(s: String): InOut = (in, out.appended(s.trim))
    def unapply: Pair[List[String]] = io

  object InOut:
    def apply(ss: String*): InOut = (ss.toList, List.empty[String])
