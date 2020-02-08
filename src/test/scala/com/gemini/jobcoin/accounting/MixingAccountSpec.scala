package com.gemini.jobcoin.accounting

import com.gemini.jobcoin.mixrequest.{MixRequestTask, MixRequestTaskState}
import org.scalatest.{FlatSpec, Matchers}

class MixingAccountSpec extends FlatSpec with Matchers {
  "MixingAccount" should "be created in Requested state" in {
    val mixingAddress = "mixing address"
    val sourceAddress = "source address"
    val destinationAddress = "dest address"
    val mixingAccount = MixingAccount.empty(mixingAddress)
    val transaction1: IdentifiableTransaction =
      Transaction.withId(
        id = "Transaction 1",
        fromAddress = sourceAddress,
        toAddress = mixingAddress,
        BigDecimal(2.2)
      )
    val mixRequestTask1: MixRequestTask =
      MixRequestTask(
        id = "Mix Request Task 1",
        mixRequestId = "Mix Request id",
        transaction = transaction1,
        state = MixRequestTaskState.Idle,
        eventHistory = Seq.empty
      )
    val sourceToMixingAccountScheduledState =
      mixingAccount.addScheduledMixRequestTasks(Seq(mixRequestTask1))
    val sourceToMixingAccountCompletedState =
      sourceToMixingAccountScheduledState.addCompletedMixRequestTasks(
        Seq(mixRequestTask1)
      )

    val transaction2: IdentifiableTransaction =
      Transaction.withId(
        id = "Transaction 2",
        fromAddress = mixingAddress,
        toAddress = destinationAddress,
        BigDecimal(2.2)
      )
    val mixRequestTask2: MixRequestTask =
      MixRequestTask(
        id = "Mix Request Task 2",
        mixRequestId = "Mix Request id",
        transaction = transaction2,
        state = MixRequestTaskState.Idle,
        eventHistory = Seq.empty
      )

    val mixingToDestinationAccountScheduledState =
      sourceToMixingAccountCompletedState.addScheduledMixRequestTasks(
        Seq(mixRequestTask2)
      )
    val mixingToDestinationAccountCompletedState =
      mixingToDestinationAccountScheduledState.addCompletedMixRequestTasks(
        Seq(mixRequestTask2)
      )

    val output: BigDecimal =
      mixingToDestinationAccountCompletedState.currentBalance
    val expectedOutput: BigDecimal = BigDecimal(0)

    output should be(expectedOutput)
  }

}
