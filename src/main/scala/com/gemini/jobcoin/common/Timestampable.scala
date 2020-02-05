package com.gemini.jobcoin.common

import java.time.LocalDateTime

trait Timestampable {
  val timestamp: LocalDateTime
}
