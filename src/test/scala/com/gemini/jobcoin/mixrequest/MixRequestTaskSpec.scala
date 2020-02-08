package com.gemini.jobcoin.mixrequest

import java.time.LocalDateTime

import com.gemini.jobcoin.accounting.{IdentifiableTransaction, Transaction}
import org.scalatest.{Assertion, FlatSpec, Matchers}

class MixRequestTaskSpec extends FlatSpec with Matchers {

  "MixRequestTask" should "should be created in idle state" in {
    val parentMixRequestId: String = "Parent Id"
    val transaction: IdentifiableTransaction =
      Transaction.withId("fromAddress", "toAddress", BigDecimal(2.2))
    val mixRequestTask: MixRequestTask =
      MixRequestTask(parentMixRequestId, transaction)
    mixRequestTask.state should be(MixRequestTaskState.Idle)
  }

  "MixRequestTask" should "should transition from idle to scheduled state" in {
    val localDateTime = LocalDateTime.now()
    val event: MixRequestTaskEvent =
      MixRequestTaskEvent.Schedule(localDateTime)

    stateTransitionTest(
      event = event,
      fromState = MixRequestTaskState.Idle,
      expectedToState = MixRequestTaskState.Scheduled
    )
  }

  "MixRequestTask" should "should remain in idle state" in {
    val localDateTime = LocalDateTime.now()
    val event: MixRequestTaskEvent =
      MixRequestTaskEvent.Reset(localDateTime)

    stateTransitionTest(
      event = event,
      fromState = MixRequestTaskState.Idle,
      expectedToState = MixRequestTaskState.Idle
    )
  }

  "MixRequestTask" should "should transition from Scheduled to Committed state" in {
    val localDateTime = LocalDateTime.now()
    val event: MixRequestTaskEvent =
      MixRequestTaskEvent.Commit(localDateTime)

    stateTransitionTest(
      event = event,
      fromState = MixRequestTaskState.Scheduled,
      expectedToState = MixRequestTaskState.Committed
    )
  }

  "MixRequestTask" should "should transition from Scheduled to Idle state" in {
    val localDateTime = LocalDateTime.now()
    val event: MixRequestTaskEvent =
      MixRequestTaskEvent.Reset(localDateTime)

    stateTransitionTest(
      event = event,
      fromState = MixRequestTaskState.Scheduled,
      expectedToState = MixRequestTaskState.Idle
    )
  }

  "MixRequestTask" should "should transition from Committed to Completed state" in {
    val localDateTime = LocalDateTime.now()
    val event: MixRequestTaskEvent =
      MixRequestTaskEvent.Complete(localDateTime)

    stateTransitionTest(
      event = event,
      fromState = MixRequestTaskState.Committed,
      expectedToState = MixRequestTaskState.Completed
    )
  }

  "MixRequestTask" should "should transition from Committed to Idle state" in {
    val localDateTime = LocalDateTime.now()
    val event: MixRequestTaskEvent =
      MixRequestTaskEvent.Reset(localDateTime)

    stateTransitionTest(
      event = event,
      fromState = MixRequestTaskState.Committed,
      expectedToState = MixRequestTaskState.Idle
    )
  }

  private def stateTransitionTest(
    event: MixRequestTaskEvent,
    fromState: MixRequestTaskState,
    expectedToState: MixRequestTaskState
  ): Assertion = {
    val parentMixRequestId: String = "Parent MixRequest Id"
    val transaction: IdentifiableTransaction =
      Transaction.withId(
        id = "Transaction ID",
        fromAddress = "fromAddress",
        toAddress = "toAddress",
        BigDecimal(2.2)
      )
    val mixRequestTaskId = "MixRequestTask Id"
    val mixRequestTask: MixRequestTask =
      MixRequestTask(
        id = mixRequestTaskId,
        mixRequestId = parentMixRequestId,
        transaction = transaction,
        state = fromState,
        eventHistory = Seq.empty
      )

    val output = mixRequestTask.transition(event)
    val expectedOutput = MixRequestTask(
      id = mixRequestTaskId,
      mixRequestId = parentMixRequestId,
      transaction = transaction,
      state = expectedToState,
      eventHistory = Seq(event)
    )
    output should be(expectedOutput)
  }

}
