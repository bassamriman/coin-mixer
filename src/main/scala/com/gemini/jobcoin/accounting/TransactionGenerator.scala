package com.gemini.jobcoin.accounting

import com.gemini.jobcoin.common.RandomNumberGenerator

import scala.annotation.tailrec

/**
  * Api that generates random transactions
  */
object TransactionGenerator {

  /**
    * Generates Random Transactions based on given addresses and amount
    * @param amountToDistribute amount to distribute from source address to destination addresses
    * @param sourceAddress address we are sending money from
    * @param destinationAddresses addresses we are distributing money too
    * @param minTransactionAmount the minimum amount per transaction
    * @param maxTransactionAmount the maximum amount per transaction
    * @param maxScale the amount of digits to the right of the decimal point
    * @param seed seed used for randomly generate numbers (useful for troubleshooting and testing)
    * @return List of transactions
    */
  def generateTransactions(
    amountToDistribute: BigDecimal,
    sourceAddress: String,
    destinationAddresses: Seq[String],
    minTransactionAmount: BigDecimal,
    maxTransactionAmount: BigDecimal,
    maxScale: Int
  )(seed: Long): Seq[IdentifiableTransaction] = {

    val transactionAmounts: Seq[BigDecimal] =
      RandomNumberGenerator.generateRandomBigDecimals(
        amountToDistribute,
        minTransactionAmount,
        maxTransactionAmount,
        maxScale
      )(seed)

    val distributions: Seq[Int] = RandomNumberGenerator.generateRandomInts(
      transactionAmounts.size,
      destinationAddresses.size
    )(seed)

    val accountToNumberOfTransactionPairs
      : Seq[(String, Int)] = destinationAddresses zip distributions

    generateTransactionsTail(
      transactionAmounts = transactionAmounts.toList,
      accountToNumberOfTransactionPairs =
        accountToNumberOfTransactionPairs.toList,
      transactions = Seq.empty,
      sourceAddress = sourceAddress
    )
  }

  @tailrec
  private def generateTransactionsTail(
    transactionAmounts: List[BigDecimal],
    accountToNumberOfTransactionPairs: List[(String, Int)],
    transactions: Seq[IdentifiableTransaction],
    sourceAddress: String
  ): Seq[IdentifiableTransaction] = {

    accountToNumberOfTransactionPairs match {
      case accountToNumberOfTransaction :: remainingAccountToNumberOfTransactionPairs =>
        val (destinationAddress, numberOfTransaction) =
          accountToNumberOfTransaction
        val destinationTransactionAmounts =
          transactionAmounts.take(numberOfTransaction)
        val newTransactionAmounts = transactionAmounts.drop(numberOfTransaction)
        val generatedTransactions =
          destinationTransactionAmounts.map(
            transactionAmount =>
              Transaction.withId(
                fromAddress = sourceAddress,
                toAddress = destinationAddress,
                amount = transactionAmount
            )
          )
        val newTransactions = transactions ++ generatedTransactions
        generateTransactionsTail(
          transactionAmounts = newTransactionAmounts,
          accountToNumberOfTransactionPairs =
            remainingAccountToNumberOfTransactionPairs,
          transactions = newTransactions,
          sourceAddress = sourceAddress
        )
      case Nil => transactions
    }
  }
}
