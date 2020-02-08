package com.gemini.jobcoin.accounting

trait Ledger[T <: Transaction, Self <: Ledger[T, Self]] {

  val transactions: Seq[T]

  def receivedAmount(address: String): BigDecimal =
    transactions
      .filter(transaction => transaction.toAddress == address)
      .map(transaction => transaction.amount)
      .sum

  def balance(address: String): BigDecimal =
    transactions.flatMap(_.signAdjustAmount(address)).sum

  def exist(transaction: T): Boolean = transactions.contains(transaction)

  def +(other: Self): Self =
    constructor(this.transactions ++ other.transactions)

  def +(transaction: T): Self = this ++ Seq(transaction)
  def ++(transactions: Seq[T]): Self = constructor(transactions ++ transactions)

  def constructor(transactions: Seq[T]): Self
}

case class BasicLedger(transactions: Seq[BasicTransaction])
    extends Ledger[BasicTransaction, BasicLedger] {
  override def constructor(transactions: Seq[BasicTransaction]): BasicLedger =
    BasicLedger(transactions)
}

case class IdentifiableTransactionLedger(
  indexedTransactions: Map[String, IdentifiableTransaction]
) extends Ledger[IdentifiableTransaction, IdentifiableTransactionLedger] {

  override val transactions: Seq[IdentifiableTransaction] =
    indexedTransactions.values.toSeq

  override def constructor(
    transactions: Seq[IdentifiableTransaction]
  ): IdentifiableTransactionLedger =
    IdentifiableTransactionLedger(transactions.map(t => t.id -> t).toMap)

  def addressInfo(address: String): StandardAccount = {
    val addressRelatedTransactions: Seq[IdentifiableTransaction] =
      transactions.filter(_.involvesAddress(address))
    StandardAccount(address, 0, constructor(addressRelatedTransactions))
  }

  def toBasicLedger: BasicLedger =
    BasicLedger(transactions.map(_.basicTransaction))
  def -(transaction: IdentifiableTransaction): IdentifiableTransactionLedger =
    this -- Seq(transaction)
  def --(
    transactions: Seq[IdentifiableTransaction]
  ): IdentifiableTransactionLedger =
    this.copy(
      indexedTransactions = indexedTransactions -- transactions.map(_.id)
    )

  override def ++(
    transactions: Seq[IdentifiableTransaction]
  ): IdentifiableTransactionLedger = this.copy(
    indexedTransactions = indexedTransactions ++ transactions
      .map(t => t.id -> t)
  )

}
/*
case class TimestampedTransactionLedger(
  transactions: Seq[TimestampableTransaction]
) extends Ledger[TimestampableTransaction, TimestampedTransactionLedger]
    with BasicLedgerDelegate {
  override def constructor(
    transactions: Seq[TimestampableTransaction]
  ): TimestampedTransactionLedger = TimestampedTransactionLedger(transactions)

}
 */
object Ledger {
  def empty: BasicLedger = BasicLedger(Seq.empty)
  /*
  def emptyTimestampedTransactionLedger: TimestampedTransactionLedger =
    TimestampedTransactionLedger(Seq.empty)
   */
  def emptyIdentifiableTransactionLedger: IdentifiableTransactionLedger =
    IdentifiableTransactionLedger(Map.empty)
}
