package com.gemini.jobcoin.dao

import java.time.ZonedDateTime

import com.gemini.jobcoin.accounting.BasicTransaction
import play.api.libs.json.{Json, Reads, Writes}

case class TransactionDAO (timestamp : ZonedDateTime,
                           fromAddress : Option[String],
                           toAddress : String,
                           amount : BigDecimal)

object TransactionDAO {
  implicit val jsonReads: Reads[TransactionDAO] = Json.reads[TransactionDAO]
}