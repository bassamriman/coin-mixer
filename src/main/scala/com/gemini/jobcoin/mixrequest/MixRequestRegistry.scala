package com.gemini.jobcoin.mixrequest

import java.time.LocalDateTime

import com.gemini.jobcoin.accounting.IdentifiableTransaction
import com.gemini.jobcoin.mixrequest.models.{
  MixRequest,
  MixRequestWithBalance,
  MixingProperties
}

import scala.annotation.tailrec
import scala.collection.immutable.TreeSet

/**
  * MixRequestRegistry controls and holds many Mix request FSMs
  * @param keyToMixRequestFSM mix request key to MixRequestFSM
  * @param sortedMixRequestsFSMByInitiatedTime sorted mix request by oldest to youngest
  * @param mixingProperties
  * @param numberOfMixRequestTaskToSchedule max number of mix request to schedule at a time
  */
case class MixRequestRegistry(
  keyToMixRequestFSM: Map[String, MixRequestFSM],
  sortedMixRequestsFSMByInitiatedTime: TreeSet[MixRequestFSM],
  mixingProperties: MixingProperties,
  numberOfMixRequestTaskToSchedule: Int,
) {

  /**
    * Check if a mix request exists in current registry
    * @param mixRequest
    * @return
    */
  def exists(mixRequest: MixRequest): Boolean =
    keyToMixRequestFSM.contains(mixRequest.id)

  /**
    * Register new mix requests
    * @param mixRequests
    * @return
    */
  def register(mixRequests: Seq[MixRequest]): MixRequestRegistry = {
    val mixRequestFSMs =
      mixRequests.map(mixRequest => MixRequestFSM(mixRequest))
    val newKeyToMixRequestFSM =
      this.keyToMixRequestFSM ++ mixRequestFSMs.map(
        mixRequestFSM => mixRequestFSM.state.mixRequest.id -> mixRequestFSM
      )
    val newSortedMixRequestsFSMByInitiatedTime = sortedMixRequestsFSMByInitiatedTime ++ mixRequestFSMs
    this.copy(
      keyToMixRequestFSM = newKeyToMixRequestFSM,
      sortedMixRequestsFSMByInitiatedTime =
        newSortedMixRequestsFSMByInitiatedTime
    )
  }

  /**
    * Register single mix request
    * @param mixRequest
    * @return new state registry
    */
  def register(mixRequest: MixRequest): MixRequestRegistry =
    register(Seq(mixRequest))

  /**
    * Update the state given mix requests to balance received
    * @param balanceMixRequestPairs
    * @param timestamp
    * @return new registry state with the update mix request task
    */
  def balanceReceived(
    balanceMixRequestPairs: Seq[(BigDecimal, MixRequest)],
    timestamp: LocalDateTime
  ): (MixRequestRegistry, Seq[MixRequestTask]) = {
    val result: Seq[(MixRequestFSM, MixRequestFSM, Seq[MixRequestTask])] =
      balanceMixRequestPairs.map { balanceMixRequestPair =>
        {
          val (balance, mixRequest) = balanceMixRequestPair
          val mixRequestFSM: MixRequestFSM = keyToMixRequestFSM(mixRequest.id)
          val event: MixRequestEvent = BalanceReceived(balance, timestamp)
          val (newMixRequestFSM, newMixRequestTasks) =
            mixRequestFSM.transition(event)
          (mixRequestFSM, newMixRequestFSM, newMixRequestTasks)
        }
      }
    updateMixRequestFSM(
      mixRequestFSMsToRemove = result.map(_._1),
      mixRequestFSMsToAdd = result.map(_._2)
    ) -> result.flatMap(_._3)
  }

  /**
    * Update the state given mix request to balance received
    * @return new registry state with the update mix request task
    */
  def balanceReceived(
    balance: BigDecimal,
    mixRequest: MixRequest,
    timestamp: LocalDateTime
  ): (MixRequestRegistry, Seq[MixRequestTask]) =
    balanceReceived(Seq((balance, mixRequest)), timestamp)

  /**
    * Update the state given mix requests to balance NOT received
    * (NOT USED)
    * @return new registry state with the update mix request task
    */
  def balanceNotReceived(
    mixRequests: Seq[MixRequest],
    timestamp: LocalDateTime
  ): (MixRequestRegistry, Seq[MixRequestTask]) = {
    val result: Seq[(MixRequestFSM, MixRequestFSM, Seq[MixRequestTask])] =
      mixRequests.map { mixRequest =>
        {
          val mixRequestFSM: MixRequestFSM = keyToMixRequestFSM(mixRequest.id)
          val event: MixRequestEvent = BalanceNotReceived(timestamp)
          val (newMixRequestFSM, newMixRequestTasks) =
            mixRequestFSM.transition(event)
          (mixRequestFSM, newMixRequestFSM, newMixRequestTasks)
        }
      }
    updateMixRequestFSM(
      mixRequestFSMsToRemove = result.map(_._1),
      mixRequestFSMsToAdd = result.map(_._2)
    ) -> result.flatMap(_._3)
  }

  /**
    * Update the state given mix request to balance NOT received
    * (NOT USED)
    * @return new registry state with the update mix request task
    */
  def balanceNotReceived(
    mixRequest: MixRequest,
    timestamp: LocalDateTime
  ): (MixRequestRegistry, Seq[MixRequestTask]) =
    balanceNotReceived(Seq(mixRequest), timestamp)

  /**
    * Updated the state of mix requests to mixing
    * @param mixRequestTransactionsPairs mix request with their corresponding transactions
    * @param timestamp
    * @return
    */
  def startMixing(
    mixRequestTransactionsPairs: Seq[
      (MixRequestWithBalance, Seq[IdentifiableTransaction])
    ],
    timestamp: LocalDateTime
  ): (MixRequestRegistry, Seq[MixRequestTask]) = {
    val result =
      mixRequestTransactionsPairs.map { mixRequestTransactionsPair =>
        {
          val (mixRequest, transactions) = mixRequestTransactionsPair
          val mixRequestFSM: MixRequestFSM = keyToMixRequestFSM(mixRequest.id)
          val event: MixRequestEvent = StartMixing(transactions, timestamp)
          val (newMixRequestFSM, newMixRequestTasks) =
            mixRequestFSM.transition(event)
          (mixRequestFSM, newMixRequestFSM, newMixRequestTasks)
        }
      }
    updateMixRequestFSM(
      mixRequestFSMsToRemove = result.map(_._1),
      mixRequestFSMsToAdd = result.map(_._2)
    ) -> result.flatMap(_._3)
  }

  /**
    * Update the state of a mix request to mixing
    * @param timestamp
    * @return
    */
  def startMixing(
    mixRequest: MixRequestWithBalance,
    transactions: Seq[IdentifiableTransaction],
    timestamp: LocalDateTime
  ): (MixRequestRegistry, Seq[MixRequestTask]) =
    startMixing(Seq((mixRequest, transactions)), timestamp)

  /**
    * Select mix request to be scheduled
    * @param timestamp
    * @param seed
    * @return
    */
  def scheduleMixRequestTasks(
    timestamp: LocalDateTime
  )(seed: Long): (MixRequestRegistry, Seq[MixRequestTask]) = {
    val idleReceivedBalanceResult
      : Set[(MixRequestFSM, MixRequestFSM, Seq[MixRequestTask])] =
      sortedMixRequestsFSMByInitiatedTime.flatMap { mixRequestFSM =>
        mixRequestFSM.state match {
          case state: ReceivedBalance =>
            if (state.isIdle) {
              val event: MixRequestEvent =
                ScheduleMixRequestTasks(
                  Seq(
                    state.mixRequest.sourceAddressToMixingAddressMixRequestTask
                  ),
                  timestamp
                )
              val (newMixRequestFSM, newMixRequestTasks) =
                mixRequestFSM.transition(event)
              Some((mixRequestFSM, newMixRequestFSM, newMixRequestTasks))
            } else {
              None
            }
          case _ => None
        }
      }

    val mixingMixRequests: Set[Mixing] =
      sortedMixRequestsFSMByInitiatedTime.flatMap { mixRequestFSM =>
        {
          mixRequestFSM.state match {
            case a: Mixing => Some(a)
            case _         => None
          }
        }
      }

    val mixRequestStatesMixRequestTaskPairsToSchedule
      : Seq[(Mixing, Seq[MixRequestTask])] =
      scheduleMixRequestTasksUntilNumberIsMet(
        mixingMixRequests.toList,
        numberOfMixRequestTaskToSchedule,
        timestamp
      )

    val idleMixingResult
      : Seq[(MixRequestFSM, MixRequestFSM, Seq[MixRequestTask])] =
      mixRequestStatesMixRequestTaskPairsToSchedule.map {
        mixRequestStatesMixRequestTaskPairToSchedule =>
          {
            val (mixRequestState, mixRequestTasks) =
              mixRequestStatesMixRequestTaskPairToSchedule
            val mixRequestFSM: MixRequestFSM =
              keyToMixRequestFSM(mixRequestState.id)
            val event: MixRequestEvent =
              ScheduleMixRequestTasks(mixRequestTasks, timestamp)
            val (newMixRequestFSM, newMixRequestTasks) =
              mixRequestFSM.transition(event)
            (mixRequestFSM, newMixRequestFSM, newMixRequestTasks)
          }
      }

    (
      updateMixRequestFSM(
        mixRequestFSMsToRemove = idleMixingResult
          .map(_._1) ++ idleReceivedBalanceResult.map(_._1),
        mixRequestFSMsToAdd = idleMixingResult
          .map(_._2) ++ idleReceivedBalanceResult.map(_._2)
      ),
      idleMixingResult.flatMap(_._3) ++ idleReceivedBalanceResult.flatMap(_._3)
    )

  }

  /**
    * Update the state of the given mix request tasks to committed
    * @param mixRequestTasks
    * @param timestamp
    * @return
    */
  def commitMixRequestTasks(
    mixRequestTasks: Seq[MixRequestTask],
    timestamp: LocalDateTime
  ): (MixRequestRegistry, Seq[MixRequestTask]) =
    transitionMixRequestTasksState(
      mixRequestTasks,
      timestamp,
      CommitMixRequestTasks
    )

  /**
    * Reset the state of the given mix request tasks
    * @param mixRequestTasks
    * @param timestamp
    * @return
    */
  def resetMixRequestTasks(
    mixRequestTasks: Seq[MixRequestTask],
    timestamp: LocalDateTime
  ): (MixRequestRegistry, Seq[MixRequestTask]) =
    transitionMixRequestTasksState(
      mixRequestTasks,
      timestamp,
      ResetMixRequestTasks
    )

  /**
    * Update the state of the given mix request tasks to completed
    * @param mixRequestTasks
    * @param timestamp
    * @return
    */
  def completeMixRequestTasks(
    mixRequestTasks: Seq[MixRequestTask],
    timestamp: LocalDateTime
  ): (MixRequestRegistry, Seq[MixRequestTask]) = transitionMixRequestTasksState(
    mixRequestTasks,
    timestamp,
    CompleteMixRequestTasks
  )

  /**
    * @Returns all the mix requests with state BalanceTransferredToMixingAddress
    */
  def getAllBalanceTransferredToMixingAddressMixRequestStates
    : Seq[BalanceTransferredToMixingAddress] = {
    sortedMixRequestsFSMByInitiatedTime
      .flatMap(
        mixRequestFSM =>
          mixRequestFSM.state match {
            case s: BalanceTransferredToMixingAddress => Some(s)
            case _                                    => None
        }
      )
      .toSeq
  }

  private def transitionMixRequestTasksState(
    mixRequestTasks: Seq[MixRequestTask],
    timestamp: LocalDateTime,
    eventConstructor: (Seq[MixRequestTask], LocalDateTime) => MixRequestEvent,
  ): (MixRequestRegistry, Seq[MixRequestTask]) = {
    val mixRequestIdToMixRequestTasks: Map[String, Seq[MixRequestTask]] =
      mixRequestTasks.groupBy(mixRequestTask => mixRequestTask.mixRequestId)
    val result = mixRequestIdToMixRequestTasks.map {
      mixRequestIdMixRequestTasksPair =>
        val (mixRequestId, mixRequestTasks) = mixRequestIdMixRequestTasksPair
        val mixRequestFSM: MixRequestFSM =
          keyToMixRequestFSM(mixRequestId)
        val event: MixRequestEvent =
          eventConstructor(mixRequestTasks, timestamp)
        val (newMixRequestFSM, newMixRequestTasks) =
          mixRequestFSM.transition(event)
        (mixRequestFSM, newMixRequestFSM, newMixRequestTasks)
    }.toSeq
    updateMixRequestFSM(
      mixRequestFSMsToRemove = result.map(_._1),
      mixRequestFSMsToAdd = result.map(_._2)
    ) -> result.flatMap(_._3)
  }

  private def updateMixRequestFSM(
    mixRequestFSMsToRemove: Seq[MixRequestFSM],
    mixRequestFSMsToAdd: Seq[MixRequestFSM]
  ): MixRequestRegistry = {
    val newKeyToMixRequestFSM =
      this.keyToMixRequestFSM -- mixRequestFSMsToRemove.map(_.id) ++ mixRequestFSMsToAdd
        .map(mixRequestFSM => mixRequestFSM.id -> mixRequestFSM)

    val newSortedMixRequestsFSMByInitiatedTime = sortedMixRequestsFSMByInitiatedTime -- mixRequestFSMsToRemove ++ mixRequestFSMsToAdd

    this.copy(
      keyToMixRequestFSM = newKeyToMixRequestFSM,
      sortedMixRequestsFSMByInitiatedTime =
        newSortedMixRequestsFSMByInitiatedTime
    )
  }

  private def scheduleMixRequestTasksUntilNumberIsMet(
    mixRequestMixingStates: List[Mixing],
    numberOfMixRequestTaskToSchedule: Int,
    timestamp: LocalDateTime
  ): Seq[(Mixing, Seq[MixRequestTask])] =
    selectMixRequestTasksUntilNumberIsMetTail(
      remainingMixRequestMixingStates = mixRequestMixingStates,
      result = Seq.empty,
      numberOfRemainingMixRequestTasks = numberOfMixRequestTaskToSchedule
    )

  //TODO: handle edge cases
  @tailrec
  private def selectMixRequestTasksUntilNumberIsMetTail(
    remainingMixRequestMixingStates: List[Mixing],
    result: Seq[(Mixing, Seq[MixRequestTask])],
    numberOfRemainingMixRequestTasks: Int
  ): Seq[(Mixing, Seq[MixRequestTask])] = {
    if (numberOfRemainingMixRequestTasks == 0) {
      result
    } else {
      remainingMixRequestMixingStates match {
        case mixRequestMixingState :: newRemainingMixRequestMixingStates =>
          val idleMixRequestTask =
            mixRequestMixingState.mixRequest.idleMixRequestTasks()
          val potentialNewNumberOfRemainingMixRequestTask = numberOfRemainingMixRequestTasks - idleMixRequestTask.size

          val (adjustedIdleMixRequestTask, newNumberOfRemainingMixRequestTask) =
            if (potentialNewNumberOfRemainingMixRequestTask >= 0) {
              (idleMixRequestTask, potentialNewNumberOfRemainingMixRequestTask)
            } else {
              val numberOfMixRequestTaskToDrop =
                Math.abs(potentialNewNumberOfRemainingMixRequestTask)
              (idleMixRequestTask.drop(numberOfMixRequestTaskToDrop), 0)
            }

          selectMixRequestTasksUntilNumberIsMetTail(
            remainingMixRequestMixingStates = newRemainingMixRequestMixingStates,
            result = result :+ (mixRequestMixingState -> adjustedIdleMixRequestTask),
            numberOfRemainingMixRequestTasks =
              newNumberOfRemainingMixRequestTask
          )
        case Nil => result
      }
    }
  }
}

object MixRequestRegistry {
  def empty(mixingProperties: MixingProperties,
            numberOfMixRequestTaskToSchedule: Int): MixRequestRegistry =
    MixRequestRegistry(
      Map.empty,
      TreeSet.empty[MixRequestFSM](MixRequestFSMOrdering),
      mixingProperties,
      numberOfMixRequestTaskToSchedule
    )
}
