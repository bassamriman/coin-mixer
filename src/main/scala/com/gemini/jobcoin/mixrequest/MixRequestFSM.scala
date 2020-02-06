package com.gemini.jobcoin.mixrequest

import java.time.LocalDateTime
import java.util.UUID

import com.gemini.jobcoin.accounting.IdentifiableTransaction

case class MixRequestFSM(state: MixRequestState, eventHistory: Seq[MixRequestEvent]) {
  val id: String = state.id
  val initiatedAt: LocalDateTime = state.initiatedAt

  def changeState(newState: MixRequestState, event: MixRequestEvent): MixRequestFSM =
    this.copy(state = newState, eventHistory = eventHistory :+ event)

  def transition(event: MixRequestEvent): (MixRequestFSM, Seq[MixRequestTask]) =
    MixRequestFSM.transition(this, event)

}

object MixRequestFSMOrdering extends Ordering[MixRequestFSM] {
  def compare(a: MixRequestFSM, b: MixRequestFSM): Int =
    a.initiatedAt.compareTo(b.initiatedAt)
}

object MixRequestFSM {
  def apply(mixRequest: MixRequest): MixRequestFSM =
    MixRequestFSM(state = Requested(mixRequest, mixRequest.initiatedAt), eventHistory = Seq.empty)

  def transition(current: MixRequestFSM, event: MixRequestEvent): (MixRequestFSM, Seq[MixRequestTask]) = {
    val (newState, newMixRequestTasks) =
      current.state match {
        case state: Requested =>
          event match {
            case e: BalanceReceived => (state.balanceReceived(e.balance, e.timestamp), Seq.empty)
            case _ => throw eventNotSupportedByStateException(event, state)
          }
        case state: ReceivedBalance =>
          event match {
            case e: Schedule => state.schedule(e.timestamp)
            case e: Commit => state.commit(e.timestamp)
            case e: Complete => (state.complete(e.timestamp), Seq.empty)
            case _ => throw eventNotSupportedByStateException(event, state)
          }
        case state: BalanceTransferredToMixingAddress =>
          event match {
            case e: StartMixing => state.startMixing(e.transactions, e.timestamp)
            case _ => throw eventNotSupportedByStateException(event, state)
          }
        case state: Mixing =>
          event match {
            case e: ScheduleMixRequestTask => state.scheduleMixRequestTask(e.mixRequestTasks, e.timestamp)
            case e: CommitMixRequestTask => state.commitMixRequestTask(e.mixRequestTasks, e.timestamp)
            case e: CompleteMixRequestTask => state.completeMixRequestTask(e.mixRequestTasks, e.timestamp)
            case e: ResetMixRequestTask => state.resetMixRequestTask(e.mixRequestTasks, e.timestamp)
            case e: MixingCompleted => (state.mixingCompleted(e.timestamp), Seq.empty)
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

  def eventNotSupportedByStateException(event: MixRequestEvent, state: MixRequestState): IllegalArgumentException =
    new IllegalArgumentException(s"State=$state doesn't support event=$event")
}

sealed trait MixRequestState {
  val mixRequest: MixRequest
  val id: String = mixRequest.id
  val initiatedAt: LocalDateTime = mixRequest.initiatedAt
}

case class Requested(mixRequest: MixRequest, requestedAt: LocalDateTime) extends MixRequestState {
  def balanceReceived(balance: BigDecimal, timestamp: LocalDateTime): ReceivedBalance =
    ReceivedBalance(MixRequestWithBalance(balance, mixRequest), timestamp)

  def balanceNotReceived(timestamp: LocalDateTime): Canceled = Canceled(mixRequest, timestamp)
}

case class Canceled(mixRequest: MixRequest, canceledAt: LocalDateTime) extends MixRequestState

case class ReceivedBalance(mixRequest: MixRequestWithBalance,
                           receivedBalanceAt: LocalDateTime) extends MixRequestState {
  val isIdle: Boolean = mixRequest.sourceAddressToMixingAddressMixRequestTask.state == Idle
  val isScheduled: Boolean = mixRequest.sourceAddressToMixingAddressMixRequestTask.state == Scheduled
  val isCompleted: Boolean = mixRequest.sourceAddressToMixingAddressMixRequestTask.state == Completed

  def schedule(timestamp: LocalDateTime): (ReceivedBalance, Seq[MixRequestTask]) =
    advanceMixRequestTaskState(timestamp)

  def commit(timestamp: LocalDateTime): (ReceivedBalance, Seq[MixRequestTask]) =
    advanceMixRequestTaskState(timestamp)

  def complete(timestamp: LocalDateTime): BalanceTransferredToMixingAddress = {
    val (newReceivedBalance, _) = advanceMixRequestTaskState(timestamp)
    BalanceTransferredToMixingAddress(newReceivedBalance.mixRequest, timestamp)
  }

  //TODO:Fix
  private def advanceMixRequestTaskState(timestamp: LocalDateTime): (ReceivedBalance, Seq[MixRequestTask]) = {
    val newSourceAddressToMixingAddressMixRequestTask = mixRequest.sourceAddressToMixingAddressMixRequestTask.nextState(timestamp)
    this.copy(
      mixRequest = MixRequestWithBalance(
        mixRequest.sourceAddressBalance,
        newSourceAddressToMixingAddressMixRequestTask,
        mixRequest.mixRequest)) -> Seq(newSourceAddressToMixingAddressMixRequestTask)
  }

}

case class BalanceTransferredToMixingAddress(mixRequest: MixRequestWithBalance,
                                             balanceTransferredAt: LocalDateTime) extends MixRequestState {
  def startMixing(transactions: Seq[IdentifiableTransaction],
                  timestamp: LocalDateTime): (Mixing, Seq[MixRequestTask]) = {
    val mixing = Mixing(MixingMixRequest(mixRequest, transactions), timestamp)
    (mixing, mixing.mixRequest.mixRequestTasks.values.toSeq)
  }
}


case class Mixing(mixRequest: MixingMixRequest, readyForMixingAt: LocalDateTime) extends MixRequestState {
  def scheduleMixRequestTask(mixRequestTasks: Seq[MixRequestTask], timestamp: LocalDateTime): (Mixing, Seq[MixRequestTask]) = {
    val (newMixRequest, newMixRequestTasks) = mixRequest.scheduleIdlePricingTask(mixRequestTasks, timestamp)
    (Mixing(newMixRequest, readyForMixingAt), newMixRequestTasks)
  }

  def commitMixRequestTask(mixRequestTasks: Seq[MixRequestTask], timestamp: LocalDateTime): (Mixing, Seq[MixRequestTask]) = ???

  def completeMixRequestTask(mixRequestTasks: Seq[MixRequestTask], timestamp: LocalDateTime): (Mixing, Seq[MixRequestTask]) = ???

  def resetMixRequestTask(mixRequestTasks: Seq[MixRequestTask], timestamp: LocalDateTime): (Mixing, Seq[MixRequestTask]) = ???

  def mixingCompleted(timestamp: LocalDateTime): MixRequestComplete = MixRequestComplete(mixRequest, timestamp)
}

case class MixRequestComplete(mixRequest: MixingMixRequest, completedAt: LocalDateTime) extends MixRequestState


sealed trait MixRequestEvent {
  val id: String
  val timestamp: LocalDateTime
}

case class BalanceNotReceived(timestamp: LocalDateTime, id: String = UUID.randomUUID().toString) extends MixRequestEvent

case class BalanceReceived(balance: BigDecimal, timestamp: LocalDateTime, id: String = UUID.randomUUID().toString) extends MixRequestEvent

case class Schedule(timestamp: LocalDateTime, id: String = UUID.randomUUID().toString) extends MixRequestEvent

case class Commit(timestamp: LocalDateTime, id: String = UUID.randomUUID().toString) extends MixRequestEvent

case class Complete(timestamp: LocalDateTime, id: String = UUID.randomUUID().toString) extends MixRequestEvent

case class StartMixing(transactions: Seq[IdentifiableTransaction], timestamp: LocalDateTime, id: String = UUID.randomUUID().toString) extends MixRequestEvent

case class ScheduleMixRequestTask(mixRequestTasks: Seq[MixRequestTask], timestamp: LocalDateTime, id: String = UUID.randomUUID().toString) extends MixRequestEvent

case class CommitMixRequestTask(mixRequestTasks: Seq[MixRequestTask], timestamp: LocalDateTime, id: String = UUID.randomUUID().toString) extends MixRequestEvent

case class CompleteMixRequestTask(mixRequestTasks: Seq[MixRequestTask], timestamp: LocalDateTime, id: String = UUID.randomUUID().toString) extends MixRequestEvent

case class ResetMixRequestTask(mixRequestTasks: Seq[MixRequestTask], timestamp: LocalDateTime, id: String = UUID.randomUUID().toString) extends MixRequestEvent

case class MixingCompleted(timestamp: LocalDateTime, id: String = UUID.randomUUID().toString) extends MixRequestEvent

