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
    val (newMixRequest, newMixRequestTasks) = mixRequest.scheduleIdlePricingTask(mixRequestTasks, timestamp)
    (Mixing(newMixRequest), newMixRequestTasks)
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


