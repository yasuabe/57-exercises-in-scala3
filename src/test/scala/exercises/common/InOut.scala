package exercises.common

import exercises.common.Pair._
import exercises.common.states._

import StateHolder.*

object InOut:
  type InOut = StateHolder[String]
  object InOut:
    def apply(ss: String*): InOut = StateHolder[String](ss*)
