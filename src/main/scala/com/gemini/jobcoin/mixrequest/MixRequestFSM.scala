package com.gemini.jobcoin.mixrequest

import java.time.LocalDateTime
import java.util.UUID

case class MixRequestFSM(state: MixRequestState, eventHistory: Seq[MixRequestEvent]) {
  val id: String = state.id
  val initiatedAt: LocalDateTime = state.initiatedAt

  def changeState(newState: MixRequestState, event: MixRequestEvent): MixRequestFSM =
    this.copy(state = newState, eventHistory = eventHistory :+ event)
}

object MixRequestFSMOrdering extends Ordering[MixRequestFSM] {
  def compare(a: MixRequestFSM, b: MixRequestFSM): Int =
    a.initiatedAt.compareTo(b.initiatedAt)
}

object MixRequestFSM {
  def apply(mixRequest: MixRequest): MixRequestFSM =
    MixRequestFSM(state = Requested(mixRequest), eventHistory = Seq.empty)

  def eventNotSupportedInState(event: MixRequestEvent, state: MixRequestState) =
    throw new IllegalStateException(s"Not Expecting Event: $event in State $state")

  //TODO: Refactor by having each state handle it's own event
  def transition(mixRequestFSM: MixRequestFSM, event: MixRequestEvent): MixRequestFSM = {
    mixRequestFSM.state match {
      case Requested(mixRequest) =>
        event match {
          case BalanceNotReceived(_, _) =>
            mixRequestFSM.copy(
              state = Canceled(mixRequest),
              eventHistory = mixRequestFSM.eventHistory :+ event)
          case BalanceReceived(balance, _, _) =>
            mixRequestFSM.copy(
              state = ReceivedBalance(MixRequestWithBalance(balance, mixRequest)),
              eventHistory = mixRequestFSM.eventHistory :+ event)
          case _ => eventNotSupportedInState(event, mixRequestFSM.state)
        }
      case Canceled(_) =>
        event match {
          case _ => eventNotSupportedInState(event, mixRequestFSM.state)
        }
      case ReceivedBalance(mixRequest) =>
        event match {
          case StartMixing(mixingProperties, seed, _, _) =>
            mixRequestFSM.copy(
              state = Mixing(MixingMixRequest(mixRequest, mixingProperties)(seed)),
              eventHistory = mixRequestFSM.eventHistory :+ event)
          case _ => eventNotSupportedInState(event, mixRequestFSM.state)
        }
      case Mixing(mixRequest) =>
        event match {
          case ScheduleMixRequestTask(mixRequestTasks: Seq[MixRequestTask], _, timestamp: LocalDateTime) =>
            mixRequestFSM.copy(
              state = Mixing(mixRequest.scheduleIdlePricingTask(mixRequestTasks, timestamp)._1),
              eventHistory = mixRequestFSM.eventHistory :+ event)
          case CommitMixRequestTask(mixRequestTasks: Seq[MixRequestTask], id: String, timestamp: LocalDateTime) => ???
            mixRequestFSM.copy(
              state = Mixing(mixRequest.commitScheduledPricingTask(mixRequestTasks, timestamp)._1),
              eventHistory = mixRequestFSM.eventHistory :+ event)
          case CompleteMixRequestTask(mixRequestTasks: Seq[MixRequestTask], id: String, timestamp: LocalDateTime) =>
            val newState = mixRequest.markCompletePricingTask(mixRequestTasks, timestamp)._1
            newState match {
              case completed: CompletedMixRequest =>
                mixRequestFSM.copy(
                  state = MixingComplete(completed),
                  eventHistory = mixRequestFSM.eventHistory :+ event)
              case other =>
                mixRequestFSM.copy(
                  state = Mixing(other),
                  eventHistory = mixRequestFSM.eventHistory :+ event)
            }
          case ResetMixRequestTask(mixRequestTasks: Seq[MixRequestTask], id: String, timestamp: LocalDateTime) =>
            mixRequestFSM.copy(
              state = Mixing(mixRequest.resetMixRequestTask(mixRequestTasks, timestamp)),
              eventHistory = mixRequestFSM.eventHistory :+ event)
          case _ => eventNotSupportedInState(event, mixRequestFSM.state)
        }
      case MixingComplete(_) =>
        event match {
          case _ => eventNotSupportedInState(event, mixRequestFSM.state)
        }
      case MixRequestComplete(_) =>
        event match {
          case _ => eventNotSupportedInState(event, mixRequestFSM.state)
        }
    }
  }
}

sealed trait MixRequestState {
  val mixRequest: MixRequest
  val id: String = mixRequest.id
  val initiatedAt: LocalDateTime = mixRequest.initiatedAt
}

case class Requested(mixRequest: MixRequest) extends MixRequestState {
  def balanceReceived(balance: BigDecimal,
                      timestamp: LocalDateTime,
                      mixingProperties: MixingProperties)(seed: Long): Mixing =
    Mixing(MixingMixRequest(MixRequestWithBalance(balance, mixRequest), mixingProperties)(seed))

  def balanceNotReceived(timestamp: LocalDateTime): Canceled = Canceled(mixRequest)
}

case class Canceled(mixRequest: MixRequest) extends MixRequestState

case class ReceivedBalance(mixRequest: MixRequestWithBalance) extends MixRequestState {
  def statMixing(balance: BigDecimal,
                 timestamp: LocalDateTime,
                 mixingProperties: MixingProperties)(seed: Long): Mixing =
    Mixing(MixingMixRequest(mixRequest, mixingProperties)(seed))
}

case class Mixing(mixRequest: MixingMixRequest) extends MixRequestState {
  def scheduleMixRequestTask(mixRequestTasks: Seq[MixRequestTask], timestamp: LocalDateTime): (Mixing, Seq[MixRequestTask]) = {
    val (newMixRequest, mixRequestTasks) = mixRequest.scheduleIdlePricingTask(mixRequestTasks, timestamp)
    (Mixing(newMixRequest), mixRequestTasks)
  }
}

case class MixingComplete(mixRequest: MixingMixRequest) extends MixRequestState {
  def validated(timestamp: LocalDateTime): MixRequestComplete =
    MixRequestComplete(CompletedMixRequest(timestamp, mixRequest))

  def validationFailed(failedMixRequestTasks: Seq[MixRequestTask], timestamp: LocalDateTime): Mixing =
    Mixing(mixRequest.resetMixRequestTask(failedMixRequestTasks, timestamp))

}

case class MixRequestComplete(mixRequest: CompletedMixRequest) extends MixRequestState


sealed trait MixRequestEvent {
  val id: String
  val timestamp: LocalDateTime
}

case class BalanceNotReceived(id: String, timestamp: LocalDateTime) extends MixRequestEvent

case class BalanceReceived(balance: BigDecimal, timestamp: LocalDateTime, id: String = UUID.randomUUID().toString) extends MixRequestEvent

case class StartMixing(mixingProperties: MixingProperties, seed: Long, id: String, timestamp: LocalDateTime) extends MixRequestEvent

case class ScheduleMixRequestTask(timestamp: LocalDateTime, id: String = UUID.randomUUID().toString) extends MixRequestEvent

case class CommitMixRequestTask(mixRequestTasks: Seq[MixRequestTask], id: String, timestamp: LocalDateTime) extends MixRequestEvent

case class CompleteMixRequestTask(mixRequestTasks: Seq[MixRequestTask], id: String, timestamp: LocalDateTime) extends MixRequestEvent

case class ResetMixRequestTask(mixRequestTasks: Seq[MixRequestTask], id: String, timestamp: LocalDateTime) extends MixRequestEvent

case class MixingCompleted(id: String, timestamp: LocalDateTime) extends MixRequestEvent


case class Validated(id: String, timestamp: LocalDateTime) extends MixRequestEvent


