package com.gemini.jobcoin.accounting

import java.time.LocalDateTime
import java.util.UUID

import com.gemini.jobcoin.common.{Identifiable, Timestampable}
import com.gemini.jobcoin.dao.TransactionDAO
import play.api.libs.json.{Json, Writes}

trait Transaction {
  val fromAddress: String
  val toAddress: String
  val amount: BigDecimal

  def involvesAddress(address: String): Boolean =
    fromAddress == address || toAddress == address

  def isReceivingAddress(address: String): Boolean = address == toAddress
  def isSendingAddress(address: String): Boolean = address == fromAddress
  def signAdjustAmount(address: String): Option[BigDecimal] =
    if (fromAddress == address && toAddress == address) {
      Some(BigDecimal(0))
    } else if (fromAddress == address) {
      Some(-amount)
    } else if (toAddress == address) {
      Some(amount)
    } else {
      None
    }
}

trait BasicTransactionDelegate extends Transaction {
  val basicTransaction: BasicTransaction
  override val fromAddress: String = basicTransaction.fromAddress
  override val toAddress: String = basicTransaction.toAddress
  override val amount: BigDecimal = basicTransaction.amount
}

case class BasicTransaction(fromAddress: String,
                            toAddress: String,
                            amount: BigDecimal)
    extends Transaction {

  def inverseSign: BasicTransaction =
    BasicTransaction(toAddress, fromAddress, -amount)

  def forcePositiveBalance(address: String): Option[BasicTransaction] = {
    if (fromAddress == address || toAddress == address) {
      if (amount < 0) {
        Some(this.inverseSign)
      } else {
        Some(this)
      }
    } else {
      None
    }
  }
}
object BasicTransaction {
  implicit val jsonWrites: Writes[BasicTransaction] =
    Json.writes[BasicTransaction]
  def apply(transaction: TransactionDAO): BasicTransaction =
    BasicTransaction(
      fromAddress = transaction.fromAddress.getOrElse(""),
      toAddress = transaction.toAddress,
      amount = transaction.amount
    )
}

case class IdentifiableTransaction(basicTransaction: BasicTransaction,
                                   id: String)
    extends BasicTransactionDelegate
    with Transaction
    with Identifiable

case class TimestampableTransaction(timestamp: LocalDateTime,
                                    basicTransaction: BasicTransaction)
    extends BasicTransactionDelegate
    with Transaction
    with Timestampable

object Transaction {
  def apply(fromAddress: String,
            toAddress: String,
            amount: BigDecimal): BasicTransaction =
    BasicTransaction(
      fromAddress = fromAddress,
      toAddress = toAddress,
      amount = amount
    )

  def withId(id: String,
             fromAddress: String,
             toAddress: String,
             amount: BigDecimal): IdentifiableTransaction =
    IdentifiableTransaction(
      Transaction(
        fromAddress = fromAddress,
        toAddress = toAddress,
        amount = amount
      ),
      id
    )

  def withId(fromAddress: String,
             toAddress: String,
             amount: BigDecimal): IdentifiableTransaction =
    withId(
      UUID.randomUUID.toString,
      fromAddress = fromAddress,
      toAddress = toAddress,
      amount = amount
    )

  def withTimestamp(timestamp: LocalDateTime,
                    fromAddress: String,
                    toAddress: String,
                    amount: BigDecimal): TimestampableTransaction =
    TimestampableTransaction(
      timestamp,
      Transaction(
        fromAddress = fromAddress,
        toAddress = toAddress,
        amount = amount
      )
    )
}
