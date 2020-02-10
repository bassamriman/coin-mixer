package com.gemini.jobcoin.accounting

import java.util.UUID

import com.gemini.jobcoin.common.Identifiable
import com.gemini.jobcoin.dao.TransactionDAO
import play.api.libs.json.{Json, Writes}

/**
  * A transaction is a transfer of funds between two address
  */
trait Transaction {
  val fromAddress: String
  val toAddress: String
  val amount: BigDecimal

  /**
    * Check if given address in either part of the from or to address of this transaction
    * @param address
    * @return
    */
  def involvesAddress(address: String): Boolean =
    fromAddress == address || toAddress == address

  /**
    * Check if given address is receiving funds due to this transaction
    * @param address
    * @return
    */
  def isReceivingAddress(address: String): Boolean = address == toAddress

  /**
    * Check if given address is sending funds due to this transaction
    * @param address
    * @return
    */
  def isSendingAddress(address: String): Boolean = address == fromAddress

  /**
    * Get the amount of this transaction with adjusted sign relative to the given address
    * @param address
    * @return
    */
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

/**
  * A simple Transaction
  * @param fromAddress
  * @param toAddress
  * @param amount
  */
case class BasicTransaction(fromAddress: String,
                            toAddress: String,
                            amount: BigDecimal)
    extends Transaction {

  /**
    * Inverse the transaction
    * @return
    */
  def inverseSign: BasicTransaction =
    BasicTransaction(toAddress, fromAddress, -amount)

  /**
    * Adjust the transaction to make sure the sign is positive based on the given Address
    * @param address
    * @return
    */
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

/**
  * Transaction with id
  * @param basicTransaction
  * @param id
  */
case class IdentifiableTransaction(basicTransaction: BasicTransaction,
                                   id: String)
    extends BasicTransactionDelegate
    with Transaction
    with Identifiable

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
}
