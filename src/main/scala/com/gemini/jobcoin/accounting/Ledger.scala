package com.gemini.jobcoin.accounting

/**
  * Ledger is a grouping of transactions.
  * @tparam T the type of transaction
  * @tparam Self the type of ledger
  */
trait Ledger[T <: Transaction, Self <: Ledger[T, Self]] {

  val transactions: Seq[T]

  /**
    * Check how much an address received based on the transactions of this ledger
    * @param address
    * @return amount
    */
  def receivedAmount(address: String): BigDecimal =
    transactions
      .filter(transaction => transaction.toAddress == address)
      .map(transaction => transaction.amount)
      .sum

  /**
    * Check the balance of an address based on the transactions of this ledger
    * @param address
    * @return amount
    */
  def balance(address: String): BigDecimal =
    transactions.flatMap(_.signAdjustAmount(address)).sum

  /**
    * Check if a transaction exits in this ledger
    * @param transaction
    * @return true or false
    */
  def exist(transaction: T): Boolean = transactions.contains(transaction)

  /**
    * Combine ledgers
    * @param other ledger
    * @return new ledger
    */
  def +(other: Self): Self =
    constructor(this.transactions ++ other.transactions)

  /**
    * Add transaction
    * @param transaction
    * @return new ledger
    */
  def +(transaction: T): Self = this ++ Seq(transaction)

  /**
    * Add transactions
    * @param transactions
    * @return new ledger
    */
  def ++(transactions: Seq[T]): Self = constructor(transactions ++ transactions)

  /**
    * Constructor of current type of ledger
    * @param transactions
    * @return new ledger
    */
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

  /**
    * All information about an address aggregated in a form of Account
    * @param address
    * @return an account
    */
  def addressInfo(address: String): StandardAccount = {
    val addressRelatedTransactions: Seq[IdentifiableTransaction] =
      transactions.filter(_.involvesAddress(address))
    StandardAccount(address, 0, constructor(addressRelatedTransactions))
  }

  /**
    * Convert to ledger that hold transactions without ID
    * @return new Basic Ledger
    */
  def toBasicLedger: BasicLedger =
    BasicLedger(transactions.map(_.basicTransaction))

  /**
    * Remove transaction based on given Id
    * @param transaction
    * @return new Ledger
    */
  def -(transaction: IdentifiableTransaction): IdentifiableTransactionLedger =
    this -- Seq(transaction)

  /**
    * Remove transactions based on given Ids
    * @param transactions
    * @return new Ledger
    */
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

object Ledger {

  def empty: BasicLedger = BasicLedger(Seq.empty)

  def emptyIdentifiableTransactionLedger: IdentifiableTransactionLedger =
    IdentifiableTransactionLedger(Map.empty)
}
