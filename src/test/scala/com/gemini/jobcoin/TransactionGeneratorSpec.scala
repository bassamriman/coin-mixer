package com.gemini.jobcoin

import com.gemini.jobcoin.accounting.{
  IdentifiableTransaction,
  TransactionGenerator
}
import org.scalatest.{FlatSpec, Matchers}

class TransactionGeneratorSpec extends FlatSpec with Matchers {
  val seed: Long = 210

  "generateTransactions method" should "TBD" in {
    val amountToDistribute = BigDecimal(74.0930)

    val sourceAddress = "Source"
    val destinationAddresses = Seq("Destination1", "Destination2")

    val minTransactionAmount = 0
    val maxTransactionAmount = 10

    val maxScale = 2

    val transactions: Seq[IdentifiableTransaction] =
      TransactionGenerator.generateTransactions(
        amountToDistribute = amountToDistribute,
        sourceAddress = sourceAddress,
        destinationAddresses = destinationAddresses,
        minTransactionAmount = minTransactionAmount,
        maxTransactionAmount = maxTransactionAmount,
        maxScale = maxScale
      )(seed)

    val output = transactions.map(_.amount).sum
    val expectedOutput = amountToDistribute
    output should be(expectedOutput)
  }

}
