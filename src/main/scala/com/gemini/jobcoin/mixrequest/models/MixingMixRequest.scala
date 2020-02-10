package com.gemini.jobcoin.mixrequest.models

import java.time.LocalDateTime

import com.gemini.jobcoin.accounting.IdentifiableTransaction
import com.gemini.jobcoin.mixrequest.{
  MixRequestTask,
  MixRequestTaskEvent,
  MixRequestTaskState
}

/**
  * Parameter used to control the mixing
  * @param minTransactionPerDestinationAddress minimum number of transactions per destination address
  * @param maxTransactionPerDestinationAddress maximum number of transactions per destination address
  * @param minTransactionAmount minimum amount of a transaction
  * @param maxTransactionAmount maximum amount of a transaction
  * @param maxScale number of digit to the right of the decimal point
  */
case class MixingProperties(minTransactionPerDestinationAddress: Int,
                            maxTransactionPerDestinationAddress: Int,
                            minTransactionAmount: BigDecimal,
                            maxTransactionAmount: BigDecimal,
                            maxScale: Int)

/**
  * Adds mix request tasks to MixRequestWithBalance.
  * These mix request tasks track the state of all the mixing transactions (in the form of mixRequestTask)
  * from mixing address to destinations addresses
  */
trait MixingMixRequest extends MixRequestWithBalanceDelegate {
  val mixRequestWithBalance: MixRequestWithBalance
  val mixRequestTasks: Map[String, MixRequestTask]

  def resetMixRequestTasks(
    mixRequestTasksToReset: Seq[MixRequestTask],
    timestamp: LocalDateTime
  ): (MixingMixRequest, Seq[MixRequestTask])

  def scheduleIdleMixRequestTasks(
    mixRequestTaskToSchedule: Seq[MixRequestTask],
    timestamp: LocalDateTime
  ): (MixingMixRequest, Seq[MixRequestTask])

  def commitScheduledMixRequestTasks(
    mixRequestTaskToCommit: Seq[MixRequestTask],
    timestamp: LocalDateTime
  ): (MixingMixRequest, Seq[MixRequestTask])

  def markCompleteMixRequestTasks(
    mixRequestTaskToComplete: Seq[MixRequestTask],
    timestamp: LocalDateTime
  ): (MixingMixRequest, Seq[MixRequestTask])

  def idleMixRequestTasks(): Seq[MixRequestTask]

  def scheduledMixRequestTasks(): Seq[MixRequestTask]

  def committedMixRequestTasks(): Seq[MixRequestTask]

  def completedMixRequestTasks(): Seq[MixRequestTask]

  def isCompleted(): Boolean
}

case class MixingMixRequestImpl(mixRequestWithBalance: MixRequestWithBalance,
                                mixRequestTasks: Map[String, MixRequestTask])
    extends MixingMixRequest
    with MixRequestWithBalance
    with MixRequest
    with MixRequestCoordinate {

  def validateMixRequestTask(
    mixRequestTaskToValidate: Seq[MixRequestTask]
  ): Unit = {
    val noneExistingMixRequestTasks =
      mixRequestTaskToValidate.filterNot(
        mixRequestTask => mixRequestTasks.contains(mixRequestTask.id)
      )
    require(
      noneExistingMixRequestTasks.isEmpty,
      s"The following MixRequestTasks $noneExistingMixRequestTasks does not exist in MixRequest $this"
    )
  }

  def validateMixRequestTasksState(
    state: MixRequestTaskState,
    mixRequestTaskToValidate: Seq[MixRequestTask]
  ): Unit = {
    val mixRequestTasksNotInGivenState =
      mixRequestTaskToValidate.filterNot(
        mixRequestTask => mixRequestTask.state == state
      )
    require(
      mixRequestTasksNotInGivenState.isEmpty,
      s"The following MixRequestTasks $mixRequestTasksNotInGivenState are not in $state"
    )
  }

  def resetMixRequestTasks(
    mixRequestTasksToReset: Seq[MixRequestTask],
    timestamp: LocalDateTime
  ): (MixingMixRequest, Seq[MixRequestTask]) = {
    transitionMixRequestTasks(
      MixRequestTaskEvent.Reset(timestamp),
      mixRequestTasksToReset,
      timestamp
    )
  }

  def scheduleIdleMixRequestTasks(
    mixRequestTasksToSchedule: Seq[MixRequestTask],
    timestamp: LocalDateTime
  ): (MixingMixRequest, Seq[MixRequestTask]) = {
    transitionMixRequestTasksWithValidation(
      MixRequestTaskEvent.Schedule(timestamp),
      MixRequestTaskState.Idle,
      mixRequestTasksToSchedule,
      timestamp
    )
  }

  def commitScheduledMixRequestTasks(
    mixRequestTasksToCommit: Seq[MixRequestTask],
    timestamp: LocalDateTime
  ): (MixingMixRequest, Seq[MixRequestTask]) = {
    transitionMixRequestTasksWithValidation(
      MixRequestTaskEvent.Commit(timestamp),
      MixRequestTaskState.Scheduled,
      mixRequestTasksToCommit,
      timestamp
    )
  }

  def markCompleteMixRequestTasks(
    mixRequestTasksToComplete: Seq[MixRequestTask],
    timestamp: LocalDateTime
  ): (MixingMixRequest, Seq[MixRequestTask]) = {
    transitionMixRequestTasksWithValidation(
      MixRequestTaskEvent.Complete(timestamp),
      MixRequestTaskState.Committed,
      mixRequestTasksToComplete,
      timestamp
    )
  }

  private def transitionMixRequestTasksWithValidation(
    event: MixRequestTaskEvent,
    expectedCurrentState: MixRequestTaskState,
    mixRequestTasksToTransitionState: Seq[MixRequestTask],
    timestamp: LocalDateTime
  ): (MixingMixRequest, Seq[MixRequestTask]) = {
    validateMixRequestTasksState(
      expectedCurrentState,
      mixRequestTasksToTransitionState
    )
    transitionMixRequestTasks(
      event = event,
      mixRequestTasksToTransitionState = mixRequestTasksToTransitionState,
      timestamp = timestamp
    )
  }

  private def transitionMixRequestTasks(
    event: MixRequestTaskEvent,
    mixRequestTasksToTransitionState: Seq[MixRequestTask],
    timestamp: LocalDateTime
  ): (MixingMixRequest, Seq[MixRequestTask]) = {
    validateMixRequestTask(mixRequestTasksToTransitionState)
    val resetMixRequestTask: Seq[(String, MixRequestTask)] =
      mixRequestTasksToTransitionState
        .map(_.transition(event))
        .map(mixRequestTask => mixRequestTask.id -> mixRequestTask)

    (
      this.copy(mixRequestTasks = this.mixRequestTasks ++ resetMixRequestTask),
      resetMixRequestTask.map(_._2)
    )

  }

  def idleMixRequestTasks(): Seq[MixRequestTask] =
    selectMixRequestTasksByState(MixRequestTaskState.Idle)

  def scheduledMixRequestTasks(): Seq[MixRequestTask] =
    selectMixRequestTasksByState(MixRequestTaskState.Scheduled)

  def committedMixRequestTasks(): Seq[MixRequestTask] =
    selectMixRequestTasksByState(MixRequestTaskState.Committed)

  def completedMixRequestTasks(): Seq[MixRequestTask] =
    selectMixRequestTasksByState(MixRequestTaskState.Completed)

  override def isCompleted(): Boolean =
    mixRequestTasks.values.forall(_.state == MixRequestTaskState.Completed)

  //TODO: Make more efficient by using map
  def selectMixRequestTasksByState(
    state: MixRequestTaskState
  ): Seq[MixRequestTask] =
    mixRequestTasks.values.filter(_.state == state).toSeq
}

object MixingMixRequest {
  def apply(mixRequestWithBalance: MixRequestWithBalance,
            transactions: Seq[IdentifiableTransaction]): MixingMixRequest = {
    val mixRequestTasks: Seq[MixRequestTask] = MixRequestTask.many(
      mixRequestId = mixRequestWithBalance.id,
      transactions = transactions
    )
    val idToMixRequestMap: Map[String, MixRequestTask] =
      Map(
        mixRequestTasks
          .map(mixRequestTask => mixRequestTask.id -> mixRequestTask): _*
      )
    MixingMixRequestImpl(mixRequestWithBalance, idToMixRequestMap)
  }
}
