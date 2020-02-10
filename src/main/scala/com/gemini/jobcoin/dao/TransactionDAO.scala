package com.gemini.jobcoin.dao

import java.time.ZonedDateTime

import play.api.libs.json.{Json, Reads}

/**
  * Transaction Data Access Object that we load from Jobcoin API
  * @param timestamp
  * @param fromAddress
  * @param toAddress
  * @param amount
  */
case class TransactionDAO(timestamp: ZonedDateTime,
                          fromAddress: Option[String],
                          toAddress: String,
                          amount: BigDecimal)

object TransactionDAO {
  implicit val jsonReads: Reads[TransactionDAO] = Json.reads[TransactionDAO]
}
