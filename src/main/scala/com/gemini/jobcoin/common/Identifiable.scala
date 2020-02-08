package com.gemini.jobcoin.common

import scala.util.Random

trait Identifiable {
  val id: String
}

case class IdGenerator(seed: Long, length: Int) {
  def generate(): String = new Random(seed).nextString(length)
}

object GiveIdentity {
  def apply[T <: Identifiable](
    constructor: String => T
  )(implicit idGenerator: IdGenerator): T =
    constructor(idGenerator.generate())
}
