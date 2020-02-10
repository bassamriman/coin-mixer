package com.gemini.jobcoin.mixrequest

import java.time.LocalDateTime
import java.util.UUID

import com.gemini.jobcoin.accounting.IdentifiableTransaction
import com.gemini.jobcoin.common.Identifiable
import com.gemini.jobcoin.mixrequest

/**
  * Mix Request Task is the building block of this entire application.
  * It holds the a transaction and it's FSM that track the transactions's state
  *
  *  idle -> scheduled -> committed -> completed (validated)
  *
  * @param id of the mix request task
  * @param mixRequestId id of the parent mix request
  * @param transaction the transaction
  * @param state state of the transaction
  * @param eventHistory history of events that cause the FSM to change state
  */
case class MixRequestTask(id: String,
                          mixRequestId: String,
                          transaction: IdentifiableTransaction,
                          state: MixRequestTaskState,
                          eventHistory: Seq[MixRequestTaskEvent])
    extends Identifiable {

  val isIdle: Boolean = state == MixRequestTaskState.Idle
  val isScheduled: Boolean = state == MixRequestTaskState.Scheduled
  val isCompleted: Boolean = state == MixRequestTaskState.Completed

  /**
    * Transition the state of the mix request task based on the given event
    * @param event
    * @return new version with he updated state
    */
  def transition(event: MixRequestTaskEvent): MixRequestTask =
    MixRequestTask.transition(this, event)

}

object MixRequestTask {
  def apply(mixRequestId: String,
            transaction: IdentifiableTransaction): MixRequestTask =
    MixRequestTask(
      id = UUID.randomUUID().toString,
      mixRequestId = mixRequestId,
      transaction = transaction,
      state = MixRequestTaskState.Idle,
      eventHistory = Seq.empty
    )

  def many(mixRequestId: String,
           transactions: Seq[IdentifiableTransaction]): Seq[MixRequestTask] =
    transactions.map(mixrequest.MixRequestTask(mixRequestId, _))

  def transition(mixRequestTask: MixRequestTask,
                 event: MixRequestTaskEvent): MixRequestTask = {
    mixRequestTask.state match {
      case s: MixRequestTaskState.Idle.type =>
        event match {
          case e: MixRequestTaskEvent.Schedule =>
            mixRequestTask.copy(
              state = MixRequestTaskState.Scheduled,
              eventHistory = mixRequestTask.eventHistory :+ e
            )
          case e: MixRequestTaskEvent.Reset =>
            mixRequestTask.copy(
              state = MixRequestTaskState.Idle,
              eventHistory = mixRequestTask.eventHistory :+ e
            )
          case otherEvent =>
            throw eventNotSupportedByStateException(otherEvent, s)
        }
      case s: MixRequestTaskState.Scheduled.type =>
        event match {
          case e: MixRequestTaskEvent.Commit =>
            mixRequestTask.copy(
              state = MixRequestTaskState.Committed,
              eventHistory = mixRequestTask.eventHistory :+ e
            )
          case e: MixRequestTaskEvent.Reset =>
            mixRequestTask.copy(
              state = MixRequestTaskState.Idle,
              eventHistory = mixRequestTask.eventHistory :+ e
            )
          case otherEvent =>
            throw eventNotSupportedByStateException(otherEvent, s)
        }
      case s: MixRequestTaskState.Committed.type =>
        event match {
          case e: MixRequestTaskEvent.Complete =>
            mixRequestTask.copy(
              state = MixRequestTaskState.Completed,
              eventHistory = mixRequestTask.eventHistory :+ e
            )
          case e: MixRequestTaskEvent.Reset =>
            mixRequestTask.copy(
              state = MixRequestTaskState.Idle,
              eventHistory = mixRequestTask.eventHistory :+ e
            )
          case otherEvent =>
            throw eventNotSupportedByStateException(otherEvent, s)
        }
      case s: MixRequestTaskState.Completed.type =>
        event match {
          case e: MixRequestTaskEvent.Reset =>
            mixRequestTask.copy(
              state = MixRequestTaskState.Idle,
              eventHistory = mixRequestTask.eventHistory :+ e
            )
          case otherEvent =>
            throw eventNotSupportedByStateException(otherEvent, s)
        }
    }
  }

  def eventNotSupportedByStateException(
    event: MixRequestTaskEvent,
    state: MixRequestTaskState
  ): IllegalArgumentException =
    new IllegalArgumentException(s"State=$state doesn't support event=$event")
}

sealed trait MixRequestTaskEvent {
  val timestamp: LocalDateTime
}
object MixRequestTaskEvent {
  case class Reset(timestamp: LocalDateTime) extends MixRequestTaskEvent
  case class Schedule(timestamp: LocalDateTime) extends MixRequestTaskEvent
  case class Commit(timestamp: LocalDateTime) extends MixRequestTaskEvent
  case class Complete(timestamp: LocalDateTime) extends MixRequestTaskEvent
}

sealed trait MixRequestTaskState
object MixRequestTaskState {
  case object Idle extends MixRequestTaskState

  case object Scheduled extends MixRequestTaskState

  case object Committed extends MixRequestTaskState

  case object Completed extends MixRequestTaskState
}
