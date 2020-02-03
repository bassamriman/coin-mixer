package com.gemini.jobcoin

import scala.annotation.tailrec

object TransactionGenerator {
  def generateTransactions(amountToDistribute: BigDecimal,
                           sourceAddress: String,
                           destinationAddresses: Seq[String],
                           minTransactionPerDestinationAddress: Int,
                           maxTransactionPerDestinationAddress: Int,
                           minTransactionAmount: BigDecimal,
                           maxTransactionAmount: BigDecimal,
                           maxScale: Int)(seed: Long): Seq[Transaction] = {
    val transactionAmounts: Seq[BigDecimal] = RandomNumberGenerator.generateRandomBigDecimals(amountToDistribute, minTransactionAmount, maxTransactionAmount, maxScale)(seed)
    val distributions: Seq[Int] = RandomNumberGenerator.generateRandomInts(transactionAmounts.size, minTransactionPerDestinationAddress, maxTransactionPerDestinationAddress)(seed)
    val accountToNumberOfTransactionPairs: Seq[(String, Int)] = destinationAddresses zip distributions

    generateTransactionsTail(
      transactionAmounts = transactionAmounts.toList,
      accountToNumberOfTransactionPairs = accountToNumberOfTransactionPairs.toList,
      transactions = Seq.empty,
      sourceAddress = sourceAddress)
  }

  @tailrec
  private def generateTransactionsTail(transactionAmounts: List[BigDecimal],
                                       accountToNumberOfTransactionPairs: List[(String, Int)],
                                       transactions: Seq[Transaction],
                                       sourceAddress: String): Seq[Transaction] = {
    accountToNumberOfTransactionPairs match {
      case accountToNumberOfTransaction :: remainingAccountToNumberOfTransactionPairs =>
        val (destinationAddress, numberOfTransaction) = accountToNumberOfTransaction
        val destinationTransactionAmounts = transactionAmounts.take(numberOfTransaction)
        val newTransactionAmounts = transactionAmounts.drop(numberOfTransaction)
        val generatedTransactions = destinationTransactionAmounts.map(transactionAmount => Transaction(fromAddress = sourceAddress, toAddress = destinationAddress, amount = transactionAmount))
        val newTransactions = transactions ++ generatedTransactions
        generateTransactionsTail(
          transactionAmounts = newTransactionAmounts,
          accountToNumberOfTransactionPairs = remainingAccountToNumberOfTransactionPairs,
          transactions = newTransactions,
          sourceAddress = sourceAddress)
      case Nil => transactions
    }
  }
}
