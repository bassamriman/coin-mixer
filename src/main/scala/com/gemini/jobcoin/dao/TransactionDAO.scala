package com.gemini.jobcoin.dao

import java.time.ZonedDateTime

import play.api.libs.json.{Json, Reads}

case class TransactionDAO(timestamp: ZonedDateTime,
                          fromAddress: Option[String],
                          toAddress: String,
                          amount: BigDecimal)

object TransactionDAO {
  implicit val jsonReads: Reads[TransactionDAO] = Json.reads[TransactionDAO]
}
