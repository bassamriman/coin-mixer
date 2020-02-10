package com.gemini.jobcoin.mixrequest

import java.time.LocalDateTime

import com.gemini.jobcoin.accounting.IdentifiableTransaction
import com.gemini.jobcoin.mixrequest.models.{
  MixRequest,
  MixRequestWithBalance,
  MixingMixRequest
}

/**
  * The finite state machine that tracks the lifecycle of a mix request.
  *  Lifecyle of a mix request:
  *  requested -> balance received -> balance transferred to mixing address -> mixing -> completed
  *
  * @param state
  * @param eventHistory
  */
case class MixRequestFSM(state: MixRequestState,
                         eventHistory: Seq[MixRequestEvent]) {
  val id: String = state.id
  val initiatedAt: LocalDateTime = state.initiatedAt

  /**
    * Changes the state to a new state
    * @param newState
    * @param event
    * @return
    */
  def changeState(newState: MixRequestState,
                  event: MixRequestEvent): MixRequestFSM =
    this.copy(state = newState, eventHistory = eventHistory :+ event)

  /**
    * Transitions to a new state based on an event
    * @param event
    * @return
    */
  def transition(event: MixRequestEvent): (MixRequestFSM, Seq[MixRequestTask]) =
    MixRequestFSM.transition(this, event)

}

object MixRequestFSMOrdering extends Ordering[MixRequestFSM] {
  def compare(a: MixRequestFSM, b: MixRequestFSM): Int =
    a.initiatedAt.compareTo(b.initiatedAt)
}

object MixRequestFSM {
  def apply(mixRequest: MixRequest): MixRequestFSM =
    MixRequestFSM(
      state = Requested(mixRequest, mixRequest.initiatedAt),
      eventHistory = Seq.empty
    )

  def transition(
    current: MixRequestFSM,
    event: MixRequestEvent
  ): (MixRequestFSM, Seq[MixRequestTask]) = {
    val (newState, newMixRequestTasks) =
      current.state match {
        case state: Requested =>
          event match {
            case e: BalanceReceived =>
              (state.balanceReceived(e.balance, e.timestamp), Seq.empty)
            case e: BalanceNotReceived =>
              (state.balanceNotReceived(e.timestamp), Seq.empty)
            case _ => throw eventNotSupportedByStateException(event, state)
          }
        case state: ReceivedBalance =>
          event match {
            case e: ScheduleMixRequestTasks =>
              state.schedule(e.mixRequestTasks, e.timestamp)
            case e: CommitMixRequestTasks =>
              state.commit(e.mixRequestTasks, e.timestamp)
            case e: CompleteMixRequestTasks =>
              state.complete(e.mixRequestTasks, e.timestamp)
            case e: ResetMixRequestTasks =>
              state.reset(e.mixRequestTasks, e.timestamp)
            case _ => throw eventNotSupportedByStateException(event, state)
          }
        case state: BalanceTransferredToMixingAddress =>
          event match {
            case e: StartMixing =>
              state.startMixing(e.transactions, e.timestamp)
            case _ => throw eventNotSupportedByStateException(event, state)
          }
        case state: Mixing =>
          event match {
            case e: ScheduleMixRequestTasks =>
              state.scheduleMixRequestTasks(e.mixRequestTasks, e.timestamp)
            case e: CommitMixRequestTasks =>
              state.commitMixRequestTasks(e.mixRequestTasks, e.timestamp)
            case e: CompleteMixRequestTasks =>
              state.completeMixRequestTask(e.mixRequestTasks, e.timestamp)
            case e: ResetMixRequestTasks =>
              state.resetMixRequestTasks(e.mixRequestTasks, e.timestamp)
            case _ => throw eventNotSupportedByStateException(event, state)
          }
        case state: MixRequestComplete =>
          event match {
            case _ => throw eventNotSupportedByStateException(event, state)
          }
        case state: Canceled =>
          event match {
            case _ => throw eventNotSupportedByStateException(event, state)
          }
      }
    (current.changeState(newState, event), newMixRequestTasks)
  }

  def eventNotSupportedByStateException(
    event: MixRequestEvent,
    state: MixRequestState
  ): IllegalArgumentException =
    new IllegalArgumentException(s"State=$state doesn't support event=$event")
}

sealed trait MixRequestState {
  val mixRequest: MixRequest
  val id: String = mixRequest.id
  val initiatedAt: LocalDateTime = mixRequest.initiatedAt
}

case class Requested(mixRequest: MixRequest, requestedAt: LocalDateTime)
    extends MixRequestState {
  def balanceReceived(balance: BigDecimal,
                      timestamp: LocalDateTime): ReceivedBalance =
    ReceivedBalance(MixRequestWithBalance(balance, mixRequest), timestamp)

  def balanceNotReceived(timestamp: LocalDateTime): Canceled =
    Canceled(mixRequest, timestamp)
}

case class Canceled(mixRequest: MixRequest, canceledAt: LocalDateTime)
    extends MixRequestState

case class ReceivedBalance(mixRequest: MixRequestWithBalance,
                           receivedBalanceAt: LocalDateTime)
    extends MixRequestState {
  val isIdle: Boolean =
    mixRequest.sourceAddressToMixingAddressMixRequestTask.isIdle
  val isScheduled: Boolean =
    mixRequest.sourceAddressToMixingAddressMixRequestTask.isScheduled
  val isCompleted: Boolean =
    mixRequest.sourceAddressToMixingAddressMixRequestTask.isCompleted

  def reset(
    mixRequestTasks: Seq[MixRequestTask],
    timestamp: LocalDateTime
  ): (ReceivedBalance, Seq[MixRequestTask]) = {
    if (mixRequestTasks
          .map(_.id)
          .contains(mixRequest.sourceAddressToMixingAddressMixRequestTask.id)) {
      transitionMixRequestTaskState(MixRequestTaskEvent.Reset(timestamp))
    } else {
      (this, Seq.empty)
    }
  }

  def schedule(
    mixRequestTasks: Seq[MixRequestTask],
    timestamp: LocalDateTime
  ): (ReceivedBalance, Seq[MixRequestTask]) = {
    if (mixRequestTasks
          .map(_.id)
          .contains(mixRequest.sourceAddressToMixingAddressMixRequestTask.id)) {
      transitionMixRequestTaskState(MixRequestTaskEvent.Schedule(timestamp))
    } else {
      (this, Seq.empty)
    }
  }

  def commit(
    mixRequestTasks: Seq[MixRequestTask],
    timestamp: LocalDateTime
  ): (ReceivedBalance, Seq[MixRequestTask]) = {
    if (mixRequestTasks
          .map(_.id)
          .contains(mixRequest.sourceAddressToMixingAddressMixRequestTask.id)) {
      transitionMixRequestTaskState(MixRequestTaskEvent.Commit(timestamp))
    } else {
      (this, Seq.empty)
    }
  }

  def complete(
    mixRequestTasks: Seq[MixRequestTask],
    timestamp: LocalDateTime
  ): (MixRequestState, Seq[MixRequestTask]) = {
    if (mixRequestTasks
          .map(_.id)
          .contains(mixRequest.sourceAddressToMixingAddressMixRequestTask.id)) {
      val (newReceivedBalance, newMixRequestTask) =
        transitionMixRequestTaskState(MixRequestTaskEvent.Complete(timestamp))
      BalanceTransferredToMixingAddress(
        newReceivedBalance.mixRequest,
        timestamp
      ) -> newMixRequestTask
    } else {
      (this, Seq.empty)
    }
  }

  private def transitionMixRequestTaskState(
    event: MixRequestTaskEvent
  ): (ReceivedBalance, Seq[MixRequestTask]) = {
    val newSourceAddressToMixingAddressMixRequestTask =
      mixRequest.sourceAddressToMixingAddressMixRequestTask.transition(event)
    this.copy(
      mixRequest = MixRequestWithBalance(
        mixRequest.sourceAddressBalance,
        newSourceAddressToMixingAddressMixRequestTask,
        mixRequest.mixRequest
      )
    ) -> Seq(newSourceAddressToMixingAddressMixRequestTask)
  }

}

case class BalanceTransferredToMixingAddress(
  mixRequest: MixRequestWithBalance,
  balanceTransferredAt: LocalDateTime
) extends MixRequestState {
  def startMixing(transactions: Seq[IdentifiableTransaction],
                  timestamp: LocalDateTime): (Mixing, Seq[MixRequestTask]) = {
    val mixing = Mixing(MixingMixRequest(mixRequest, transactions), timestamp)
    (mixing, mixing.mixRequest.mixRequestTasks.values.toSeq)
  }
}

case class Mixing(mixRequest: MixingMixRequest, readyForMixingAt: LocalDateTime)
    extends MixRequestState {
  def scheduleMixRequestTasks(
    mixRequestTasks: Seq[MixRequestTask],
    timestamp: LocalDateTime
  ): (Mixing, Seq[MixRequestTask]) = {
    val (newMixRequest, newMixRequestTasks) =
      mixRequest.scheduleIdleMixRequestTasks(mixRequestTasks, timestamp)
    (Mixing(newMixRequest, readyForMixingAt), newMixRequestTasks)
  }

  def commitMixRequestTasks(
    mixRequestTasks: Seq[MixRequestTask],
    timestamp: LocalDateTime
  ): (Mixing, Seq[MixRequestTask]) = {
    val (newMixRequest, newMixRequestTasks) =
      mixRequest.commitScheduledMixRequestTasks(mixRequestTasks, timestamp)
    (Mixing(newMixRequest, readyForMixingAt), newMixRequestTasks)
  }

  def completeMixRequestTask(
    mixRequestTasks: Seq[MixRequestTask],
    timestamp: LocalDateTime
  ): (MixRequestState, Seq[MixRequestTask]) = {
    val (newMixRequest, newMixRequestTasks) =
      mixRequest.markCompleteMixRequestTasks(mixRequestTasks, timestamp)
    if (newMixRequest.isCompleted()) {
      (MixRequestComplete(newMixRequest, timestamp), newMixRequestTasks)
    } else {
      (Mixing(newMixRequest, readyForMixingAt), newMixRequestTasks)
    }
  }

  def resetMixRequestTasks(
    mixRequestTasks: Seq[MixRequestTask],
    timestamp: LocalDateTime
  ): (Mixing, Seq[MixRequestTask]) = {
    val (newMixRequest, newMixRequestTasks) =
      mixRequest.resetMixRequestTasks(mixRequestTasks, timestamp)
    (Mixing(newMixRequest, readyForMixingAt), newMixRequestTasks)
  }

  def mixingCompleted(timestamp: LocalDateTime): MixRequestComplete =
    MixRequestComplete(mixRequest, timestamp)
}

case class MixRequestComplete(mixRequest: MixingMixRequest,
                              completedAt: LocalDateTime)
    extends MixRequestState

sealed trait MixRequestEvent extends {
  val timestamp: LocalDateTime
}

case class BalanceNotReceived(timestamp: LocalDateTime) extends MixRequestEvent

case class BalanceReceived(balance: BigDecimal, timestamp: LocalDateTime)
    extends MixRequestEvent

case class StartMixing(transactions: Seq[IdentifiableTransaction],
                       timestamp: LocalDateTime)
    extends MixRequestEvent

case class ScheduleMixRequestTasks(mixRequestTasks: Seq[MixRequestTask],
                                   timestamp: LocalDateTime)
    extends MixRequestEvent

case class CommitMixRequestTasks(mixRequestTasks: Seq[MixRequestTask],
                                 timestamp: LocalDateTime)
    extends MixRequestEvent

case class CompleteMixRequestTasks(mixRequestTasks: Seq[MixRequestTask],
                                   timestamp: LocalDateTime)
    extends MixRequestEvent

case class ResetMixRequestTasks(mixRequestTasks: Seq[MixRequestTask],
                                timestamp: LocalDateTime)
    extends MixRequestEvent
