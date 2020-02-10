package com.gemini.jobcoin.actors

import java.time.LocalDateTime
import java.util.UUID

import akka.actor.{ActorRef, Props}
import com.gemini.jobcoin.common.MixerActor
import com.gemini.jobcoin.mixrequest.models.{
  MixRequest,
  MixRequestWithBalance,
  MixingProperties
}
import com.gemini.jobcoin.mixrequest.{models, _}

/**
  * AccountAndMixRequestManagerActor is responsible for managing all the state.
  * AccountAndMixRequestManagerActor is also responsible for coordinating the execution
  * of all steps required to successfully mix coins in to different addresses.
  *
  * @param mixingAddress The address where all coins will get mixed
  * @param mixingProperties Parameters that controls the random transactions generator
  * @param initialSeed Seed to control randomness of the random number generation (makes it easy to reproduce issues)
  * @param balanceMonitorActor Actor that is responsible for detecting when money is sent to a specific address
  * @param mixedTransactionGeneratorActor Actor responsible for generating random transactions
  * @param committerActor Actor that is responsible for persisting transactions
  * @param validatorActor Actor responsible for validating if a transaction was persisted or not
  */
case class AccountAndMixRequestManagerActor(
  mixingAddress: String,
  mixingProperties: MixingProperties,
  numberOfMixRequestTaskToSchedule: Int,
  initialSeed: Long,
  balanceMonitorActor: ActorRef,
  mixedTransactionGeneratorActor: ActorRef,
  committerActor: ActorRef,
  validatorActor: ActorRef
) extends MixerActor {

  import AccountAndMixRequestManagerActor._

  override def receive: Receive =
    logged(
      handle(
        MixRequestAccountManager.empty(
          mixingAddress,
          mixingProperties,
          numberOfMixRequestTaskToSchedule
        ),
        initialSeed
      )
    )

  def handle(mixRequestAccountManager: MixRequestAccountManager,
             seed: Long): Receive = {
    case NewRequestDispatcherActor
          .NewRequests(mixRequestCoordinates, timestamp) =>
      val newMixRequests: Seq[MixRequest] = mixRequestCoordinates.map(
        mixRequestCoordinate =>
          models.MixRequest(mixingAddress, timestamp, mixRequestCoordinate)
      )
      balanceMonitorActor ! BalanceMonitorActor.NewRequestsAwaitingBalance(
        newMixRequests
      )
      val newMixRequestAccountManager =
        mixRequestAccountManager.registerNewMixRequest(newMixRequests)
      context.become(
        logged(
          handle(
            mixRequestAccountManager = newMixRequestAccountManager,
            seed = seed
          )
        )
      )

    case BalanceMonitorActor
          .BalanceReceived(balanceMixRequestPairs, timestamp) =>
      val (newMixRequestAccountManager, _) =
        mixRequestAccountManager.balanceReceivedFor(
          balanceMixRequestPairs,
          timestamp
        )
      context.become(
        logged(
          handle(
            mixRequestAccountManager = newMixRequestAccountManager,
            seed = seed
          )
        )
      )

    case BalanceMonitorActor.BalanceNotReceived(mixRequests, timestamp) =>
      val (newMixRequestAccountManager, _) =
        mixRequestAccountManager.balanceNotReceived(mixRequests, timestamp)
      context.become(
        logged(
          handle(
            mixRequestAccountManager = newMixRequestAccountManager,
            seed = seed
          )
        )
      )

    case MixedTransactionGeneratorActor.MixedTransactions(
        mixRequestTransactionsPairs,
        timestamp: LocalDateTime
        ) =>
      val (newMixRequestAccountManager, _) =
        mixRequestAccountManager.startMixing(
          mixRequestTransactionsPairs,
          timestamp
        )
      context.become(
        logged(
          handle(
            mixRequestAccountManager = newMixRequestAccountManager,
            seed = seed
          )
        )
      )

    case ScheduleNewMixRequestTasks =>
      val (newMixRequestAccountManager, scheduledMixRequestTasks) =
        mixRequestAccountManager.scheduleMixRequestTasks(LocalDateTime.now())(
          seed
        )
      scheduledMixRequestTasks.foreach(
        committerActor ! CommitterActor.Commit(_)
      )
      context.become(
        logged(
          handle(
            mixRequestAccountManager = newMixRequestAccountManager,
            seed = seed + 1
          )
        )
      )

    case CommitterActor.Committed(mixRequestTask, timestamp) =>
      val (newMixRequestAccountManager, scheduledMixRequestTasks) =
        mixRequestAccountManager.commitMixRequestTasks(
          mixRequestTask,
          timestamp
        )
      validatorActor ! ValidatorActor.Validate(scheduledMixRequestTasks)
      context.become(
        logged(
          handle(
            mixRequestAccountManager = newMixRequestAccountManager,
            seed = seed
          )
        )
      )

    case CommitterActor.FailedToCommit(mixRequestTask, timestamp) =>
      val (newMixRequestAccountManager, _) =
        mixRequestAccountManager.rollBackToScheduling(mixRequestTask, timestamp)
      context.become(
        logged(
          handle(
            mixRequestAccountManager = newMixRequestAccountManager,
            seed = seed
          )
        )
      )

    case ValidatorActor.Validated(mixRequestTask, timestamp) =>
      val newMixRequestAccountManager =
        mixRequestAccountManager.completeMixRequestTasks(
          mixRequestTask,
          timestamp
        )

      val balanceTransferredToMixingAddressMixRequests
        : Seq[MixRequestWithBalance] =
        newMixRequestAccountManager.mixRequestRegistry.getAllBalanceTransferredToMixingAddressMixRequestStates
          .map(_.mixRequest)

      if (balanceTransferredToMixingAddressMixRequests.nonEmpty) {
        mixedTransactionGeneratorActor !
          MixedTransactionGeneratorActor.GenerateMixedTransactions(
            mixingAddress,
            balanceTransferredToMixingAddressMixRequests
          )
      }

      context.become(
        logged(
          handle(
            mixRequestAccountManager = newMixRequestAccountManager,
            seed = seed
          )
        )
      )
    case AccountAndMixRequestManagerActor.GetStatus =>
      sender() ! AccountAndMixRequestManagerActor.Status(
        mixRequestAccountManager
      )
  }

}

object AccountAndMixRequestManagerActor {
  def props(address: String,
            mixingProperties: MixingProperties,
            numberOfMixRequestTaskToSchedule: Int,
            initialSeed: Long,
            balanceMonitorActor: ActorRef,
            mixedTransactionGeneratorActor: ActorRef,
            committerActor: ActorRef,
            validatorActor: ActorRef): Props =
    Props(
      AccountAndMixRequestManagerActor(
        mixingAddress = address,
        mixingProperties = mixingProperties,
        numberOfMixRequestTaskToSchedule = numberOfMixRequestTaskToSchedule,
        initialSeed = initialSeed,
        balanceMonitorActor = balanceMonitorActor,
        mixedTransactionGeneratorActor = mixedTransactionGeneratorActor,
        committerActor = committerActor,
        validatorActor = validatorActor
      )
    )

  def props(mixingProperties: MixingProperties,
            numberOfMixRequestTaskToSchedule: Int,
            initialSeed: Long,
            balanceMonitorActor: ActorRef,
            mixedTransactionGeneratorActor: ActorRef,
            committerActor: ActorRef,
            validatorActor: ActorRef): Props =
    props(
      address = UUID.randomUUID().toString,
      mixingProperties = mixingProperties,
      numberOfMixRequestTaskToSchedule = numberOfMixRequestTaskToSchedule,
      initialSeed = initialSeed,
      balanceMonitorActor = balanceMonitorActor,
      mixedTransactionGeneratorActor = mixedTransactionGeneratorActor,
      committerActor = committerActor,
      validatorActor = validatorActor
    )

  case object ScheduleNewMixRequestTasks

  case object GetStatus
  case class Status(mixRequestAccountManager: MixRequestAccountManager)

}
