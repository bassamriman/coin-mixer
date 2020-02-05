package com.gemini.jobcoin.accounting

trait Ledger[T <: Transaction, Self <: Ledger[T, Self]] {

  val transactions: Seq[T]

  def receivedAmount(address: String): BigDecimal =
    transactions.filter(transaction => transaction.toAddress == address).map(transaction => transaction.amount).sum

  def balance(address: String): BigDecimal = transactions.flatMap(_.signAdjustAmount(address)).sum

  def exist(transaction: T): Boolean = transactions.contains(transaction)

  def +(other: Self): Self = constructor(this.transactions ++ other.transactions)

  def +(transaction: T): Self = constructor(transactions :+ transaction)

  def constructor(transactions: Seq[T]): Self
}

trait BasicLedgerDelegate {
  val transactions: Seq[BasicTransactionDelegate]

  def toBasicLedger: BasicLedger = BasicLedger(transactions.map(_.basicTransaction))
}

case class BasicLedger(transactions: Seq[BasicTransaction]) extends Ledger[BasicTransaction, BasicLedger] {
  override def constructor(transactions: Seq[BasicTransaction]): BasicLedger = BasicLedger(transactions)
}

case class IdentifiableTransactionLedger(transactions: Seq[IdentifiableTransaction])
  extends Ledger[IdentifiableTransaction, IdentifiableTransactionLedger]
    with BasicLedgerDelegate {
  override def constructor(transactions: Seq[IdentifiableTransaction]): IdentifiableTransactionLedger = IdentifiableTransactionLedger(transactions)

  def addressInfo(address: String): StandardAccount = {
    val addressRelatedTransactions: Seq[IdentifiableTransaction] = transactions.filter(_.involvesAddress(address))
    StandardAccount(address, 0, constructor(addressRelatedTransactions))
  }
}

case class TimestampedTransactionLedger(transactions: Seq[TimestampableTransaction])
  extends Ledger[TimestampableTransaction, TimestampedTransactionLedger]
    with BasicLedgerDelegate {
  override def constructor(transactions: Seq[TimestampableTransaction]): TimestampedTransactionLedger = TimestampedTransactionLedger(transactions)

}

object Ledger {
  def empty: BasicLedger = BasicLedger(Seq.empty)

  def emptyTimestampedTransactionLedger: TimestampedTransactionLedger = TimestampedTransactionLedger(Seq.empty)

  def emptyIdentifiableTransactionLedger: IdentifiableTransactionLedger = IdentifiableTransactionLedger(Seq.empty)
}
