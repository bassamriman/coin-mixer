package com.gemini.jobcoin.mixrequest

import java.time.LocalDateTime

import com.gemini.jobcoin.accounting.{IdentifiableTransaction, Transaction}
import com.gemini.jobcoin.mixrequest.models.{
  MixRequest,
  MixRequestCoordinate,
  MixRequestWithBalanceImpl,
  MixingMixRequestImpl
}
import org.scalatest.{FlatSpec, Matchers}

class MixRequestFSMSpec extends FlatSpec with Matchers {

  val sourceAddress: String = "Source Address"
  val mixingAddress: String = "Mixing Address"
  val destinationAddress1: String = "Destination Address 1"
  val destinationAddress2: String = "Destination Address 2"

  val mixRequest: MixRequest = {
    val mixRequestId: String = "MixRequest Id"

    val initiatedAt: LocalDateTime = LocalDateTime.now()
    val mixRequestCoordinate: MixRequestCoordinate = MixRequestCoordinate(
      sourceAddress,
      Seq(destinationAddress1, destinationAddress2)
    )
    MixRequest(
      id = mixRequestId,
      mixingAddress = mixingAddress,
      initiatedAt = initiatedAt,
      mixRequestCoordinate = mixRequestCoordinate
    )
  }

  "MixRequestFSM" should "be created in Requested state" in {
    val output: MixRequestFSM = MixRequestFSM(mixRequest)
    val expectedOutput: MixRequestFSM =
      MixRequestFSM(Requested(mixRequest, mixRequest.initiatedAt), Seq.empty)

    output should be(expectedOutput)
  }

  "MixRequestFSM" should "transition from Requested to BalanceReceived state" in {
    val balanceToReceive: BigDecimal = BigDecimal(34.3)
    val receivedTime: LocalDateTime = LocalDateTime.now()

    val fromState: MixRequestState =
      Requested(mixRequest, mixRequest.initiatedAt)

    val event: MixRequestEvent =
      BalanceReceived(balanceToReceive, receivedTime)

    val output =
      MixRequestFSM(fromState, Seq.empty).transition(event)

    val mixRequestTaskId =
      output._1.state
        .asInstanceOf[ReceivedBalance]
        .mixRequest
        .sourceAddressToMixingAddressMixRequestTask
        .id

    val transactionId =
      output._1.state
        .asInstanceOf[ReceivedBalance]
        .mixRequest
        .sourceAddressToMixingAddressMixRequestTask
        .transaction
        .id

    val transaction: IdentifiableTransaction =
      Transaction.withId(
        id = transactionId,
        fromAddress = sourceAddress,
        toAddress = mixingAddress,
        balanceToReceive
      )

    val mixRequestTask: MixRequestTask =
      MixRequestTask(
        id = mixRequestTaskId,
        mixRequestId = mixRequest.id,
        transaction = transaction,
        state = MixRequestTaskState.Idle,
        eventHistory = Seq.empty
      )
    val expectedToState: MixRequestState = ReceivedBalance(
      MixRequestWithBalanceImpl(balanceToReceive, mixRequestTask, mixRequest),
      receivedTime
    )

    val expectedOutput: MixRequestFSM =
      MixRequestFSM(expectedToState, Seq(event))

    output should be((expectedOutput, Seq.empty))
  }

  "MixRequestFSM" should "transition from Requested to BalanceNotReceived state" in {
    val receivedTime: LocalDateTime = LocalDateTime.now()

    val fromState: MixRequestState =
      Requested(mixRequest, mixRequest.initiatedAt)
    val expectedToState: MixRequestState = Canceled(mixRequest, receivedTime)

    val event: MixRequestEvent =
      BalanceNotReceived(receivedTime)

    val output =
      MixRequestFSM(fromState, Seq.empty).transition(event)
    val expectedOutput: MixRequestFSM =
      MixRequestFSM(expectedToState, Seq(event))

    output should be((expectedOutput, Seq.empty))
  }

  "MixRequestFSM" should "transition from BalanceReceived with Idle Task to Scheduled Task state" in {
    val receivedTime: LocalDateTime = LocalDateTime.now()

    val balanceToReceive: BigDecimal = BigDecimal(34.3)

    val mixRequestTaskId = "Mix Request Task ID"

    val transactionId = "Transaction Id"

    val transaction: IdentifiableTransaction =
      Transaction.withId(
        id = transactionId,
        fromAddress = sourceAddress,
        toAddress = mixingAddress,
        balanceToReceive
      )

    val mixRequestTask: MixRequestTask =
      MixRequestTask(
        id = mixRequestTaskId,
        mixRequestId = mixRequest.id,
        transaction = transaction,
        state = MixRequestTaskState.Idle,
        eventHistory = Seq.empty
      )

    val mixRequestWithBalance: MixRequestWithBalanceImpl =
      models.MixRequestWithBalanceImpl(
        balanceToReceive,
        mixRequestTask,
        mixRequest
      )

    val fromState: MixRequestState =
      ReceivedBalance(mixRequestWithBalance, receivedTime)

    val expectedMixRequestTask = mixRequestTask.copy(
      state = MixRequestTaskState.Scheduled,
      eventHistory = Seq(MixRequestTaskEvent.Schedule(receivedTime))
    )
    val expectedToState: ReceivedBalance =
      ReceivedBalance(
        mixRequestWithBalance.copy(
          sourceAddressToMixingAddressMixRequestTask = expectedMixRequestTask
        ),
        receivedTime
      )

    val event: MixRequestEvent =
      ScheduleMixRequestTasks(
        Seq(mixRequestWithBalance.sourceAddressToMixingAddressMixRequestTask),
        receivedTime
      )

    val output = MixRequestFSM(fromState, Seq.empty).transition(event)

    val expectedOutput: MixRequestFSM =
      MixRequestFSM(expectedToState, Seq(event))

    output should be((expectedOutput, Seq(expectedMixRequestTask)))
  }

  "MixRequestFSM" should "transition from BalanceReceived with Scheduled Task to Commit Task state" in {
    val receivedTime: LocalDateTime = LocalDateTime.now()

    val balanceToReceive: BigDecimal = BigDecimal(34.3)

    val mixRequestTaskId = "Mix Request Task ID"

    val transactionId = "Transaction Id"

    val transaction: IdentifiableTransaction =
      Transaction.withId(
        id = transactionId,
        fromAddress = sourceAddress,
        toAddress = mixingAddress,
        balanceToReceive
      )

    val mixRequestTask: MixRequestTask =
      MixRequestTask(
        id = mixRequestTaskId,
        mixRequestId = mixRequest.id,
        transaction = transaction,
        state = MixRequestTaskState.Scheduled,
        eventHistory = Seq.empty
      )

    val mixRequestWithBalance: MixRequestWithBalanceImpl =
      models.MixRequestWithBalanceImpl(
        balanceToReceive,
        mixRequestTask,
        mixRequest
      )

    val fromState: MixRequestState =
      ReceivedBalance(mixRequestWithBalance, receivedTime)

    val expectedMixRequestTask = mixRequestTask.copy(
      state = MixRequestTaskState.Committed,
      eventHistory = Seq(MixRequestTaskEvent.Commit(receivedTime))
    )
    val expectedToState: ReceivedBalance =
      ReceivedBalance(
        mixRequestWithBalance.copy(
          sourceAddressToMixingAddressMixRequestTask = expectedMixRequestTask
        ),
        receivedTime
      )

    val event: MixRequestEvent =
      CommitMixRequestTasks(
        Seq(mixRequestWithBalance.sourceAddressToMixingAddressMixRequestTask),
        receivedTime
      )

    val output = MixRequestFSM(fromState, Seq.empty).transition(event)

    val expectedOutput: MixRequestFSM =
      MixRequestFSM(expectedToState, Seq(event))

    output should be((expectedOutput, Seq(expectedMixRequestTask)))
  }

  "MixRequestFSM" should "transition from BalanceReceived to BalanceTransferredToMixingAddress state" in {
    val receivedTime: LocalDateTime = LocalDateTime.now()

    val balanceToReceive: BigDecimal = BigDecimal(34.3)

    val mixRequestTaskId = "Mix Request Task ID"

    val transactionId = "Transaction Id"

    val transaction: IdentifiableTransaction =
      Transaction.withId(
        id = transactionId,
        fromAddress = sourceAddress,
        toAddress = mixingAddress,
        balanceToReceive
      )

    val mixRequestTask: MixRequestTask =
      MixRequestTask(
        id = mixRequestTaskId,
        mixRequestId = mixRequest.id,
        transaction = transaction,
        state = MixRequestTaskState.Committed,
        eventHistory = Seq.empty
      )

    val mixRequestWithBalance: MixRequestWithBalanceImpl =
      models.MixRequestWithBalanceImpl(
        balanceToReceive,
        mixRequestTask,
        mixRequest
      )

    val fromState: MixRequestState =
      ReceivedBalance(mixRequestWithBalance, receivedTime)

    val expectedMixRequestTask = mixRequestTask.copy(
      state = MixRequestTaskState.Completed,
      eventHistory = Seq(MixRequestTaskEvent.Complete(receivedTime))
    )
    val expectedToState: BalanceTransferredToMixingAddress =
      BalanceTransferredToMixingAddress(
        mixRequestWithBalance.copy(
          sourceAddressToMixingAddressMixRequestTask = expectedMixRequestTask
        ),
        receivedTime
      )

    val event: MixRequestEvent =
      CompleteMixRequestTasks(
        Seq(mixRequestWithBalance.sourceAddressToMixingAddressMixRequestTask),
        receivedTime
      )

    val output = MixRequestFSM(fromState, Seq.empty).transition(event)

    val expectedOutput: MixRequestFSM =
      MixRequestFSM(expectedToState, Seq(event))

    output should be((expectedOutput, Seq(expectedMixRequestTask)))
  }

  "MixRequestFSM" should "transition from BalanceTransferredToMixingAddress to Mixing state" in {
    val receivedTime: LocalDateTime = LocalDateTime.now()

    val balanceToReceive: BigDecimal = BigDecimal(34.3)

    val mixRequestTaskId = "Mix Request Task ID"

    val transactionId = "Transaction Id"

    val transaction: IdentifiableTransaction =
      Transaction.withId(
        id = transactionId,
        fromAddress = sourceAddress,
        toAddress = mixingAddress,
        balanceToReceive
      )

    val mixRequestTask: MixRequestTask =
      MixRequestTask(
        id = mixRequestTaskId,
        mixRequestId = mixRequest.id,
        transaction = transaction,
        state = MixRequestTaskState.Completed,
        eventHistory = Seq.empty
      )

    val mixRequestWithBalance: MixRequestWithBalanceImpl =
      models.MixRequestWithBalanceImpl(
        balanceToReceive,
        mixRequestTask,
        mixRequest
      )

    val fromState: MixRequestState =
      BalanceTransferredToMixingAddress(mixRequestWithBalance, receivedTime)

    val transactions: Seq[IdentifiableTransaction] =
      Seq(
        Transaction
          .withId(
            "Transaction Id",
            "Source",
            "Destination 1",
            BigDecimal(23.21)
          )
      )
    val readyForMixingAt: LocalDateTime = LocalDateTime.now()
    val event: MixRequestEvent = StartMixing(transactions, readyForMixingAt)
    val output = MixRequestFSM(fromState, Seq.empty).transition(event)
    val expectedMixRequestTasks: Map[String, MixRequestTask] =
      output._1.state.mixRequest
        .asInstanceOf[MixingMixRequestImpl]
        .mixRequestTasks
        .map(
          entry =>
            entry._1 -> entry._2.copy(
              mixRequestId = mixRequest.id,
              transaction = transactions.head,
              state = MixRequestTaskState.Idle,
              eventHistory = Seq.empty
          )
        )

    val expectedToState: Mixing =
      Mixing(
        MixingMixRequestImpl(mixRequestWithBalance, expectedMixRequestTasks),
        readyForMixingAt
      )

    val expectedOutput: MixRequestFSM =
      MixRequestFSM(expectedToState, Seq(event))

    output should be((expectedOutput, expectedMixRequestTasks.values.toStream))
  }

  "MixRequestFSM" should "transition from Mixing Idle to Scheduled state" in {

    val balanceToReceive: BigDecimal = BigDecimal(34.3)

    val mixRequestTaskId = "Mix Request Task ID"

    val transactionId = "Transaction Id"

    val transaction: IdentifiableTransaction =
      Transaction.withId(
        id = transactionId,
        fromAddress = sourceAddress,
        toAddress = mixingAddress,
        balanceToReceive
      )

    val mixRequestTask: MixRequestTask =
      MixRequestTask(
        id = mixRequestTaskId,
        mixRequestId = mixRequest.id,
        transaction = transaction,
        state = MixRequestTaskState.Completed,
        eventHistory = Seq.empty
      )

    val mixRequestWithBalance: MixRequestWithBalanceImpl =
      models.MixRequestWithBalanceImpl(
        balanceToReceive,
        mixRequestTask,
        mixRequest
      )

    val transactions: Seq[IdentifiableTransaction] =
      Seq(
        Transaction
          .withId(
            "Transaction Id",
            "Source",
            "Destination 1",
            BigDecimal(23.21)
          )
      )
    val readyForMixingAt: LocalDateTime = LocalDateTime.now()

    val mixRequestTasks: Map[String, MixRequestTask] = transactions
      .map(transaction => MixRequestTask(mixRequestWithBalance.id, transaction))
      .map(m => m.id -> m)
      .toMap
    val fromState: MixRequestState = Mixing(
      MixingMixRequestImpl(mixRequestWithBalance, mixRequestTasks),
      readyForMixingAt
    )
    val scheduledAt: LocalDateTime = LocalDateTime.now
    val expectMixRequestTasks: Map[String, MixRequestTask] =
      mixRequestTasks.mapValues(
        mr =>
          mr.copy(
            state = MixRequestTaskState.Scheduled,
            eventHistory = Seq(MixRequestTaskEvent.Schedule(scheduledAt))
        )
      )
    val expectedToState: Mixing =
      Mixing(
        MixingMixRequestImpl(mixRequestWithBalance, expectMixRequestTasks),
        readyForMixingAt
      )

    val event: MixRequestEvent =
      ScheduleMixRequestTasks(mixRequestTasks.values.toSeq, scheduledAt)

    val output = MixRequestFSM(fromState, Seq.empty).transition(event)
    val expectedOutput: MixRequestFSM =
      MixRequestFSM(expectedToState, Seq(event))

    output should be((expectedOutput, expectMixRequestTasks.values.toStream))
  }

  "MixRequestFSM" should "transition from Mixing Scheduled to Committed state" in {

    val balanceToReceive: BigDecimal = BigDecimal(34.3)

    val mixRequestTaskId = "Mix Request Task ID"

    val transactionId = "Transaction Id"

    val transaction: IdentifiableTransaction =
      Transaction.withId(
        id = transactionId,
        fromAddress = sourceAddress,
        toAddress = mixingAddress,
        balanceToReceive
      )

    val mixRequestTask: MixRequestTask =
      MixRequestTask(
        id = mixRequestTaskId,
        mixRequestId = mixRequest.id,
        transaction = transaction,
        state = MixRequestTaskState.Completed,
        eventHistory = Seq.empty
      )

    val mixRequestWithBalance: MixRequestWithBalanceImpl =
      models.MixRequestWithBalanceImpl(
        balanceToReceive,
        mixRequestTask,
        mixRequest
      )

    val transactions: Seq[IdentifiableTransaction] =
      Seq(
        Transaction
          .withId(
            "Transaction Id",
            "Source",
            "Destination 1",
            BigDecimal(23.21)
          )
      )
    val readyForMixingAt: LocalDateTime = LocalDateTime.now()

    val mixRequestTasks: Map[String, MixRequestTask] = transactions
      .map(
        transaction =>
          MixRequestTask(
            id = "Mix Request Id",
            mixRequestId = mixRequestWithBalance.id,
            transaction = transaction,
            state = MixRequestTaskState.Scheduled,
            eventHistory = Seq.empty
        )
      )
      .map(m => m.id -> m)
      .toMap
    val fromState: MixRequestState = Mixing(
      MixingMixRequestImpl(mixRequestWithBalance, mixRequestTasks),
      readyForMixingAt
    )
    val commitAt: LocalDateTime = LocalDateTime.now
    val expectMixRequestTasks: Map[String, MixRequestTask] =
      mixRequestTasks.mapValues(
        mr =>
          mr.copy(
            state = MixRequestTaskState.Committed,
            eventHistory = Seq(MixRequestTaskEvent.Commit(commitAt))
        )
      )
    val expectedToState: Mixing =
      Mixing(
        MixingMixRequestImpl(mixRequestWithBalance, expectMixRequestTasks),
        readyForMixingAt
      )

    val event: MixRequestEvent =
      CommitMixRequestTasks(mixRequestTasks.values.toSeq, commitAt)

    val output = MixRequestFSM(fromState, Seq.empty).transition(event)
    val expectedOutput: MixRequestFSM =
      MixRequestFSM(expectedToState, Seq(event))

    output should be((expectedOutput, expectMixRequestTasks.values.toStream))
  }

  "MixRequestFSM" should "transition from Mixing Committed to Completed state" in {

    val balanceToReceive: BigDecimal = BigDecimal(34.3)

    val mixRequestTaskId = "Mix Request Task ID"

    val transactionId = "Transaction Id"

    val transaction: IdentifiableTransaction =
      Transaction.withId(
        id = transactionId,
        fromAddress = sourceAddress,
        toAddress = mixingAddress,
        balanceToReceive
      )

    val mixRequestTask: MixRequestTask =
      MixRequestTask(
        id = mixRequestTaskId,
        mixRequestId = mixRequest.id,
        transaction = transaction,
        state = MixRequestTaskState.Completed,
        eventHistory = Seq.empty
      )

    val mixRequestWithBalance: MixRequestWithBalanceImpl =
      models.MixRequestWithBalanceImpl(
        balanceToReceive,
        mixRequestTask,
        mixRequest
      )

    val transactions: Seq[IdentifiableTransaction] =
      Seq(
        Transaction
          .withId(
            "Transaction Id",
            "Source",
            "Destination 1",
            BigDecimal(23.21)
          )
      )
    val readyForMixingAt: LocalDateTime = LocalDateTime.now()

    val mixRequestTasks: Map[String, MixRequestTask] = transactions
      .map(
        transaction =>
          MixRequestTask(
            id = "Mix Request Id",
            mixRequestId = mixRequestWithBalance.id,
            transaction = transaction,
            state = MixRequestTaskState.Committed,
            eventHistory = Seq.empty
        )
      )
      .map(m => m.id -> m)
      .toMap
    val fromState: MixRequestState = Mixing(
      MixingMixRequestImpl(mixRequestWithBalance, mixRequestTasks),
      readyForMixingAt
    )
    val completeAt: LocalDateTime = LocalDateTime.now
    val expectMixRequestTasks: Map[String, MixRequestTask] =
      mixRequestTasks.mapValues(
        mr =>
          mr.copy(
            state = MixRequestTaskState.Completed,
            eventHistory = Seq(MixRequestTaskEvent.Complete(completeAt))
        )
      )
    val expectedToState =
      MixRequestComplete(
        MixingMixRequestImpl(mixRequestWithBalance, expectMixRequestTasks),
        completeAt
      )

    val event: MixRequestEvent =
      CompleteMixRequestTasks(mixRequestTasks.values.toSeq, completeAt)

    val output = MixRequestFSM(fromState, Seq.empty).transition(event)
    val expectedOutput: MixRequestFSM =
      MixRequestFSM(expectedToState, Seq(event))

    output should be((expectedOutput, expectMixRequestTasks.values.toStream))
  }

  "MixRequestFSM" should "remain in Mixing Committed state as not mixRequestTasks are completed" in {

    val balanceToReceive: BigDecimal = BigDecimal(34.3)

    val mixRequestTaskId = "Mix Request Task ID"

    val transactionId = "Transaction Id"

    val transaction: IdentifiableTransaction =
      Transaction.withId(
        id = transactionId,
        fromAddress = sourceAddress,
        toAddress = mixingAddress,
        balanceToReceive
      )

    val mixRequestTask: MixRequestTask =
      MixRequestTask(
        id = mixRequestTaskId,
        mixRequestId = mixRequest.id,
        transaction = transaction,
        state = MixRequestTaskState.Completed,
        eventHistory = Seq.empty
      )

    val mixRequestWithBalance: MixRequestWithBalanceImpl =
      models.MixRequestWithBalanceImpl(
        balanceToReceive,
        mixRequestTask,
        mixRequest
      )

    val transaction2: IdentifiableTransaction =
      Transaction
        .withId("Transaction Id", "Source", "Destination 1", BigDecimal(23.21))

    val readyForMixingAt: LocalDateTime = LocalDateTime.now()

    val committedMixRequest1 = MixRequestTask(
      id = "Mix Request Id 1",
      mixRequestId = mixRequestWithBalance.id,
      transaction = transaction2,
      state = MixRequestTaskState.Committed,
      eventHistory = Seq.empty
    )

    val committedMixRequest2 = MixRequestTask(
      id = "Mix Request Id 2",
      mixRequestId = mixRequestWithBalance.id,
      transaction = transaction2,
      state = MixRequestTaskState.Committed,
      eventHistory = Seq.empty
    )

    val mixRequestTasks: Map[String, MixRequestTask] =
      Seq(
        committedMixRequest1.id -> committedMixRequest1,
        committedMixRequest2.id -> committedMixRequest2
      ).toMap

    val fromState: MixRequestState = Mixing(
      MixingMixRequestImpl(mixRequestWithBalance, mixRequestTasks),
      readyForMixingAt
    )
    val completeAt: LocalDateTime = LocalDateTime.now

    val expectNewCommittedMixRequest2: MixRequestTask =
      committedMixRequest2.copy(
        state = MixRequestTaskState.Completed,
        eventHistory = Seq(MixRequestTaskEvent.Complete(completeAt))
      )

    val expectMixRequestTasks: Map[String, MixRequestTask] =
      Seq(
        committedMixRequest1.id -> committedMixRequest1,
        committedMixRequest2.id -> expectNewCommittedMixRequest2
      ).toMap

    val expectedToState: Mixing =
      Mixing(
        MixingMixRequestImpl(mixRequestWithBalance, expectMixRequestTasks),
        readyForMixingAt
      )

    val event: MixRequestEvent =
      CompleteMixRequestTasks(Seq(committedMixRequest2), completeAt)

    val output = MixRequestFSM(fromState, Seq.empty).transition(event)
    val expectedOutput: MixRequestFSM =
      MixRequestFSM(expectedToState, Seq(event))

    output should be(
      (expectedOutput, Seq(expectNewCommittedMixRequest2).toStream)
    )
  }

}
