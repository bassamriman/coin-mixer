package com.gemini.jobcoin.mixrequest

import java.time.LocalDateTime

import com.gemini.jobcoin.accounting.TransactionGenerator

case class MixingProperties(minTransactionPerDestinationAddress: Int,
                            maxTransactionPerDestinationAddress: Int,
                            minTransactionAmount: BigDecimal,
                            maxTransactionAmount: BigDecimal,
                            maxScale: Int,
                            numberOfMixRequestTaskToSchedule: Int)

trait MixingMixRequest extends MixRequestWithBalanceDelegate {
  val mixRequestWithBalance: MixRequestWithBalance
  val mixRequestTasks: Map[String, MixRequestTask]

  def resetMixRequestTask(mixRequestTaskToReset: Seq[MixRequestTask], timestamp: LocalDateTime): MixingMixRequest

  def scheduleIdlePricingTask(mixRequestTaskToSchedule: Seq[MixRequestTask], timestamp: LocalDateTime): (MixingMixRequest, Seq[MixRequestTask])

  def commitScheduledPricingTask(mixRequestTaskToCommit: Seq[MixRequestTask], timestamp: LocalDateTime): (MixingMixRequest, Seq[MixRequestTask])

  def markCompletePricingTask(mixRequestTaskToComplete: Seq[MixRequestTask], timestamp: LocalDateTime): (CompletedMixRequest, Seq[MixRequestTask])

  def changePricingStateToNextState(mixRequestTaskToChangeToNextState: Seq[MixRequestTask], timestamp: LocalDateTime): (MixingMixRequest, Seq[MixRequestTask])

  def idlePricingTasks(): Seq[MixRequestTask]

  def scheduledPricingTasks(): Seq[MixRequestTask]

  def committedPricingTasks(): Seq[MixRequestTask]

  def completedPricingTasks(): Seq[MixRequestTask]
}

case class MixingMixRequestImpl(mixRequestWithBalance: MixRequestWithBalance,
                                mixRequestTasks: Map[String, MixRequestTask])
  extends MixingMixRequest
    with MixRequestWithBalance
    with MixRequest
    with MixRequestCoordinate {

  def validateMixRequestTask(mixRequestTaskToValidate: Seq[MixRequestTask]): Unit = {
    val noneExistingMixRequestTasks =
      mixRequestTaskToValidate.filterNot(mixRequestTask => mixRequestTasks.contains(mixRequestTask.id))
    require(noneExistingMixRequestTasks.nonEmpty,
      s"The following MixRequestTasks $noneExistingMixRequestTasks does not exist in MixRequest $this")
  }

  def validateMixRequestTaskState(state: MixRequestTaskState, mixRequestTaskToValidate: Seq[MixRequestTask]): Unit = {
    val mixRequestTasksNotInGivenState =
      mixRequestTaskToValidate.filterNot(mixRequestTask => mixRequestTask.state == state)
    require(mixRequestTasksNotInGivenState.nonEmpty,
      s"The following MixRequestTasks $mixRequestTasksNotInGivenState are not in $state")
  }

  def resetMixRequestTask(mixRequestTaskToReset: Seq[MixRequestTask], timestamp: LocalDateTime): MixingMixRequest = {
    validateMixRequestTask(mixRequestTaskToReset)

    val resetMixRequestTask = mixRequestTaskToReset
      .map(_.reset(timestamp))
      .map(mixRequestTask => mixRequestTask.id -> mixRequestTask)
    this.copy(mixRequestTasks = this.mixRequestTasks ++ resetMixRequestTask)
  }

  def scheduleIdlePricingTask(mixRequestTaskToSchedule: Seq[MixRequestTask], timestamp: LocalDateTime): (MixingMixRequest, Seq[MixRequestTask]) = {
    validateMixRequestTaskState(Idle, mixRequestTaskToSchedule)
    changePricingStateToNextState(mixRequestTaskToSchedule, timestamp)
  }

  def commitScheduledPricingTask(mixRequestTaskToCommit: Seq[MixRequestTask], timestamp: LocalDateTime): (MixingMixRequest, Seq[MixRequestTask]) = {
    validateMixRequestTaskState(Scheduled, mixRequestTaskToCommit)
    changePricingStateToNextState(mixRequestTaskToCommit, timestamp)
  }

  def markCompletePricingTask(mixRequestTaskToComplete: Seq[MixRequestTask], timestamp: LocalDateTime): (CompletedMixRequest, Seq[MixRequestTask]) = {
    validateMixRequestTaskState(Committed, mixRequestTaskToComplete)
    val (newMixingMixRequest, updatedMixRequestTask) = changePricingStateToNextState(mixRequestTaskToComplete, timestamp)
    (CompletedMixRequest(timestamp, newMixingMixRequest), updatedMixRequestTask)
  }

  def changePricingStateToNextState(mixRequestTaskToChangeToNextState: Seq[MixRequestTask], timestamp: LocalDateTime): (MixingMixRequest, Seq[MixRequestTask]) = {
    validateMixRequestTask(mixRequestTaskToChangeToNextState)

    val resetMixRequestTask: Seq[(String, MixRequestTask)] = mixRequestTaskToChangeToNextState
      .map(_.nextState(timestamp))
      .map(mixRequestTask => mixRequestTask.id -> mixRequestTask)

    (this.copy(mixRequestTasks = this.mixRequestTasks ++ resetMixRequestTask), resetMixRequestTask.map(_._2))

  }

  def idlePricingTasks(): Seq[MixRequestTask] = selectPricingTasksByState(Idle)

  def scheduledPricingTasks(): Seq[MixRequestTask] = selectPricingTasksByState(Scheduled)

  def committedPricingTasks(): Seq[MixRequestTask] = selectPricingTasksByState(Committed)

  def completedPricingTasks(): Seq[MixRequestTask] = selectPricingTasksByState(Completed)


  //TODO: Make more efficient by using map
  def selectPricingTasksByState(state: MixRequestTaskState): Seq[MixRequestTask] = mixRequestTasks.values.filter(_.state == state).toSeq
}

object MixingMixRequest {
  def apply(mixRequestWithBalance: MixRequestWithBalance, mixingProperties: MixingProperties)(seed: Long): MixingMixRequest = {
    val mixRequestTasks: Seq[MixRequestTask] = MixRequestTask.many(
      mixRequestId = mixRequestWithBalance.id,
      transactions = TransactionGenerator.generateTransactions(
        amountToDistribute = mixRequestWithBalance.sourceAddressBalance,
        sourceAddress = mixRequestWithBalance.sourceAddress,
        destinationAddresses = mixRequestWithBalance.destinationAddresses,
        minTransactionPerDestinationAddress = mixingProperties.minTransactionPerDestinationAddress,
        maxTransactionPerDestinationAddress = mixingProperties.maxTransactionPerDestinationAddress,
        minTransactionAmount = mixingProperties.minTransactionAmount,
        maxTransactionAmount = mixingProperties.maxTransactionAmount,
        maxScale = mixingProperties.maxScale
      )(seed))

    val idToMixRequestMap: Map[String, MixRequestTask] =
      Map(mixRequestTasks.map(mixRequestTask => mixRequestTask.id -> mixRequestTask): _*)
    MixingMixRequestImpl(mixRequestWithBalance, idToMixRequestMap)
  }
}
