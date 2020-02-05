package com.gemini.jobcoin.mixrequest

import java.time.LocalDateTime
import java.util.UUID

import com.gemini.jobcoin.accounting.IdentifiableTransaction
import com.gemini.jobcoin.common.Identifiable
import com.gemini.jobcoin.mixrequest

case class MixRequestTask(id: String,
                          mixRequestId: String,
                          transaction: IdentifiableTransaction,
                          state: MixRequestTaskState,
                          eventHistory: Seq[MixRequestTaskEvent]) extends Identifiable {
  def reset(timestamp: LocalDateTime): MixRequestTask =
    this.copy(state = Idle, eventHistory = eventHistory :+ Reset(state, timestamp))

  def nextState(timestamp: LocalDateTime): MixRequestTask = {
    val from: MixRequestTaskState = state.nextState()
    val to: MixRequestTaskState = state.nextState()
    this.copy(
      state = state.nextState(),
      eventHistory = eventHistory :+ NextStateTransition(from = from, to = to, timestamp = timestamp))
  }
}

object MixRequestTask {
  def apply(mixRequestId: String, transaction: IdentifiableTransaction): MixRequestTask =
    MixRequestTask(
      id = UUID.randomUUID().toString,
      mixRequestId = mixRequestId,
      transaction = transaction,
      state = Idle,
      eventHistory = Seq.empty)

  def many(mixRequestId: String, transactions: Seq[IdentifiableTransaction]): Seq[MixRequestTask] =
    transactions.map(mixrequest.MixRequestTask(mixRequestId, _))
}

trait MixRequestTaskEvent {
  val id: String
  val timestamp: LocalDateTime
}

case class Reset(from: MixRequestTaskState, id: String, timestamp: LocalDateTime) extends MixRequestTaskEvent

object Reset {
  def apply(from: MixRequestTaskState, timestamp: LocalDateTime): Reset =
    Reset(from, UUID.randomUUID().toString, timestamp)
}

case class NextStateTransition(from: MixRequestTaskState,
                               to: MixRequestTaskState,
                               id: String,
                               timestamp: LocalDateTime) extends MixRequestTaskEvent

object NextStateTransition {
  def apply(from: MixRequestTaskState, to: MixRequestTaskState, timestamp: LocalDateTime): NextStateTransition =
    NextStateTransition(from, to, UUID.randomUUID().toString, timestamp)
}

sealed trait MixRequestTaskState {
  def nextState(): MixRequestTaskState
}

object Idle extends MixRequestTaskState {
  override def nextState(): MixRequestTaskState = Scheduled
}

object Scheduled extends MixRequestTaskState {
  override def nextState(): MixRequestTaskState = Committed
}

object Committed extends MixRequestTaskState {
  override def nextState(): MixRequestTaskState = Completed
}

object Completed extends MixRequestTaskState {
  override def nextState(): MixRequestTaskState = Completed
}