package com.gemini.jobcoin.mixrequest

import java.time.LocalDateTime

import scala.annotation.tailrec
import scala.collection.immutable.TreeSet
import scala.util.Random

case class MixRequestRegistry(keyToMixRequestFSM: Map[String, MixRequestFSM],
                              sortedMixRequestsFSMByInitiatedTime: TreeSet[MixRequestFSM],
                              mixingProperties: MixingProperties) {

  def exists(mixRequest: MixRequest): Boolean = keyToMixRequestFSM.contains(mixRequest.id)

  def register(mixRequests: Seq[MixRequest]): MixRequestRegistry = {
    val mixRequestFSMs = mixRequests.map(mixRequest => MixRequestFSM(mixRequest))
    this.copy(
      keyToMixRequestFSM = this.keyToMixRequestFSM ++ mixRequestFSMs.map(mixRequestFSM => mixRequestFSM.state.mixRequest.id -> mixRequestFSM),
      sortedMixRequestsFSMByInitiatedTime = sortedMixRequestsFSMByInitiatedTime ++ mixRequestFSMs
    )
  }

  def register(mixRequest: MixRequest): MixRequestRegistry = register(Seq(mixRequest))

  def balanceReceived(balance: BigDecimal, mixRequest: MixRequest, timestamp: LocalDateTime)(seed: Long): Option[MixRequestRegistry] = {
    keyToMixRequestFSM.get(mixRequest.id).map {
      oldMixRequestFSM => {
        val newState = oldMixRequestFSM.state match {
          case mixRequestState: Requested => mixRequestState.balanceReceived(balance, timestamp, mixingProperties)(seed)
          case otherState => otherState
        }

        val event: MixRequestEvent = BalanceReceived(balance, timestamp)

        val (oldMixRequestFSMs, newMixRequestFSMs) = updateStateOfMixRequestFSM(Seq(newState), event, keyToMixRequestFSM)
        updateMixRequestFSM(oldMixRequestFSMs, newMixRequestFSMs)
      }
    }
  }


  def balanceNotReceived(mixRequest: MixRequest, timestamp: LocalDateTime): Option[MixRequestRegistry] = ???


  def scheduleMixRequestTasks(timestamp: LocalDateTime)(seed: Long): (MixRequestRegistry, Seq[MixRequestTask]) = {
    val mixRequestStates: Set[Mixing] = sortedMixRequestsFSMByInitiatedTime.flatMap {
      mixRequestFSM => {
        mixRequestFSM.state match {
          case a: Mixing => Some(a)
          case _ => None
        }
      }
    }

    val (selectedMixRequestStates, selectedMixRequestTask) =
      scheduleMixRequestTasksUntilNumberIsMet(mixRequestStates.toList, mixingProperties.numberOfMixRequestTaskToSchedule, timestamp)

    val event: MixRequestEvent = ScheduleMixRequestTask(timestamp)

    val (oldMixRequestFSMs, newMixRequestFSMs) = updateStateOfMixRequestFSM(selectedMixRequestStates, event, keyToMixRequestFSM)

    val shuffledSelectedMixRequestTask = new Random(seed).shuffle(selectedMixRequestTask)

    (updateMixRequestFSM(oldMixRequestFSMs, newMixRequestFSMs), shuffledSelectedMixRequestTask)
  }

  def updateMixRequestFSM(mixRequestFSMsToRemove: Seq[MixRequestFSM], mixRequestFSMsToAdd: Seq[MixRequestFSM]): MixRequestRegistry = {
    val newKeyToMixRequestFSM =
      this.keyToMixRequestFSM -- mixRequestFSMsToRemove.map(_.id) ++ mixRequestFSMsToAdd.map(mixRequestFSM => mixRequestFSM.id -> mixRequestFSM)

    val newSortedMixRequestsFSMByInitiatedTime = sortedMixRequestsFSMByInitiatedTime -- mixRequestFSMsToRemove ++ mixRequestFSMsToAdd

    this.copy(keyToMixRequestFSM = newKeyToMixRequestFSM,
      sortedMixRequestsFSMByInitiatedTime = newSortedMixRequestsFSMByInitiatedTime)
  }

  def commitMixRequestTask(mixRequestTasks: Seq[MixRequestTask],
                           timestamp: LocalDateTime): (MixRequestRegistry, Seq[MixRequestTask]) = ???

  def completeMixRequestTask(mixRequestTasks: Seq[MixRequestTask],
                             timestamp: LocalDateTime): (MixRequestRegistry, Seq[MixRequestTask]) = ???

  private def updateStateOfMixRequestFSM(newStates: Seq[MixRequestState],
                                         event: MixRequestEvent,
                                         mixRequestFSMs: Map[String, MixRequestFSM]): (Seq[MixRequestFSM], Seq[MixRequestFSM]) = {
    newStates.flatMap(newState => mixRequestFSMs.get(newState.id).map(oldFSM => (oldFSM, newState))).map {
      oldFSMNewStatePair =>
        val (oldFSM, newState) = oldFSMNewStatePair
        (oldFSM, oldFSM.changeState(newState, event))
    }.unzip
  }

  private def scheduleMixRequestTasksUntilNumberIsMet(mixRequestMixingState: List[Mixing],
                                                      numberOfMixRequestTaskToSchedule: Int,
                                                      timestamp: LocalDateTime): (Seq[Mixing], Seq[MixRequestTask]) =
    scheduleMixRequestTasksUntilNumberIsMetTail(
      remainingMixRequestMixingState = mixRequestMixingState,
      accumulatedMixRequestMixingState = List.empty,
      accumulatedMixRequestTask = Seq.empty,
      numberOfRemainingMixRequestTasks = numberOfMixRequestTaskToSchedule,
      timestamp = timestamp)


  //TODO: handle edge cases
  @tailrec
  private def scheduleMixRequestTasksUntilNumberIsMetTail(remainingMixRequestMixingState: List[Mixing],
                                                          accumulatedMixRequestMixingState: Seq[Mixing],
                                                          accumulatedMixRequestTask: Seq[MixRequestTask],
                                                          numberOfRemainingMixRequestTasks: Int,
                                                          timestamp: LocalDateTime): (Seq[Mixing], Seq[MixRequestTask]) = {
    if (numberOfRemainingMixRequestTasks == 0) {
      (accumulatedMixRequestMixingState, accumulatedMixRequestTask)
    } else {
      remainingMixRequestMixingState match {
        case mixRequestMixingState :: newRemainingMixRequestMixingState =>
          val idleMixRequestTask = mixRequestMixingState.mixRequest.idlePricingTasks()
          val potentialNewNumberOfRemainingMixRequestTask = numberOfRemainingMixRequestTasks - idleMixRequestTask.size

          val ((newMixRequestMixingState, newMixRequestTasks), newNumberOfRemainingMixRequestTask) =
            if (potentialNewNumberOfRemainingMixRequestTask >= 0) {
              (mixRequestMixingState.scheduleMixRequestTask(idleMixRequestTask, timestamp), potentialNewNumberOfRemainingMixRequestTask)
            } else {
              val numberOfMixRequestTaskToDrop = Math.abs(potentialNewNumberOfRemainingMixRequestTask)
              val newIdleMixRequestTask = idleMixRequestTask.drop(numberOfMixRequestTaskToDrop)
              (mixRequestMixingState.scheduleMixRequestTask(newIdleMixRequestTask, timestamp), 0)
            }

          scheduleMixRequestTasksUntilNumberIsMetTail(
            remainingMixRequestMixingState = newRemainingMixRequestMixingState,
            accumulatedMixRequestMixingState = accumulatedMixRequestMixingState :+ newMixRequestMixingState,
            accumulatedMixRequestTask = accumulatedMixRequestTask ++ newMixRequestTasks,
            numberOfRemainingMixRequestTasks = newNumberOfRemainingMixRequestTask,
            timestamp = timestamp)
        case Nil => (accumulatedMixRequestMixingState, accumulatedMixRequestTask)
      }
    }

  }

}


object MixRequestRegistry {
  def empty(mixingProperties: MixingProperties): MixRequestRegistry =
    MixRequestRegistry(
      Map.empty(MixRequestFSMOrdering),
      TreeSet.empty[MixRequestFSM](MixRequestFSMOrdering),
      mixingProperties)
}