package com.gemini.jobcoin.mixrequest

import java.time.LocalDateTime

import com.gemini.jobcoin.accounting.IdentifiableTransaction

import scala.annotation.tailrec
import scala.collection.immutable.TreeSet

case class MixRequestRegistry(
  keyToMixRequestFSM: Map[String, MixRequestFSM],
  sortedMixRequestsFSMByInitiatedTime: TreeSet[MixRequestFSM],
  mixingProperties: MixingProperties
) {

  def exists(mixRequest: MixRequest): Boolean =
    keyToMixRequestFSM.contains(mixRequest.id)

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

  def register(mixRequest: MixRequest): MixRequestRegistry =
    register(Seq(mixRequest))

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

  def balanceReceived(
    balance: BigDecimal,
    mixRequest: MixRequest,
    timestamp: LocalDateTime
  ): (MixRequestRegistry, Seq[MixRequestTask]) =
    balanceReceived(Seq((balance, mixRequest)), timestamp)

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

  def balanceNotReceived(
    mixRequest: MixRequest,
    timestamp: LocalDateTime
  ): (MixRequestRegistry, Seq[MixRequestTask]) =
    balanceNotReceived(Seq(mixRequest), timestamp)

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

  def startMixing(
    mixRequest: MixRequestWithBalance,
    transactions: Seq[IdentifiableTransaction],
    timestamp: LocalDateTime
  ): (MixRequestRegistry, Seq[MixRequestTask]) =
    startMixing(Seq((mixRequest, transactions)), timestamp)

  def scheduleMixRequestTasks(
    timestamp: LocalDateTime
  )(seed: Long): (MixRequestRegistry, Seq[MixRequestTask]) = {
    val idleReceivedBalanceResult
      : Set[(MixRequestFSM, MixRequestFSM, Seq[MixRequestTask])] =
      sortedMixRequestsFSMByInitiatedTime.flatMap { mixRequestFSM =>
        mixRequestFSM.state match {
          case state: ReceivedBalance =>
            if (state.isIdle) {
              val event: MixRequestEvent = Schedule(timestamp)
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
        mixingProperties.numberOfMixRequestTaskToSchedule,
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
              ScheduleMixRequestTask(mixRequestTasks, timestamp)
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

  def updateMixRequestFSM(
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

  def commitMixRequestTask(
    mixRequestTasks: Seq[MixRequestTask],
    timestamp: LocalDateTime
  ): (MixRequestRegistry, Seq[MixRequestTask]) = ???

  def rollBackToScheduling(
    mixRequestTasks: Seq[MixRequestTask],
    timestamp: LocalDateTime
  ): (MixRequestRegistry, Seq[MixRequestTask]) = ???

  def completeMixRequestTask(
    mixRequestTasks: Seq[MixRequestTask],
    timestamp: LocalDateTime
  ): (MixRequestRegistry, Seq[MixRequestTask]) = ???

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
            mixRequestMixingState.mixRequest.idlePricingTasks()
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
  def empty(mixingProperties: MixingProperties): MixRequestRegistry =
    MixRequestRegistry(
      Map.empty(MixRequestFSMOrdering),
      TreeSet.empty[MixRequestFSM](MixRequestFSMOrdering),
      mixingProperties
    )
}
