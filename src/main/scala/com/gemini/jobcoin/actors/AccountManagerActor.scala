package com.gemini.jobcoin.actors

import java.time.LocalDateTime

import akka.actor.{ActorRef, Props}
import com.gemini.jobcoin.common.MixerActor
import com.gemini.jobcoin.mixrequest._

case class AccountManagerActor(mixingAddress: String,
                               mixingProperties: MixingProperties,
                               initialSeed: Long,
                               balanceMonitorActor: ActorRef,
                               mixedTransactionGeneratorActor: ActorRef,
                               committerActor: ActorRef,
                               validatorActor: ActorRef)
    extends MixerActor {

  import AccountManagerActor._

  override def receive: Receive =
    logged(
      handle(
        MixRequestAccountManager.empty(mixingAddress, mixingProperties),
        initialSeed
      )
    )

  def handle(mixRequestAccountManager: MixRequestAccountManager,
             seed: Long): Receive = {
    case NewRequestDispatcherActor
          .NewRequests(mixRequestCoordinates, timestamp) =>
      val newMixRequests: Seq[MixRequest] = mixRequestCoordinates.map(
        mixRequestCoordinate =>
          MixRequest(mixingAddress, timestamp, mixRequestCoordinate)
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
    case AccountManagerActor.GetStatus =>
      sender() ! AccountManagerActor.Status(mixRequestAccountManager)
  }

}

object AccountManagerActor {
  def props(address: String,
            mixingProperties: MixingProperties,
            initialSeed: Long,
            balanceMonitorActor: ActorRef,
            mixedTransactionGeneratorActor: ActorRef,
            committerActor: ActorRef,
            validatorActor: ActorRef): Props =
    Props(
      AccountManagerActor(
        mixingAddress = address,
        mixingProperties = mixingProperties,
        initialSeed = initialSeed,
        balanceMonitorActor = balanceMonitorActor,
        mixedTransactionGeneratorActor = mixedTransactionGeneratorActor,
        committerActor = committerActor,
        validatorActor = validatorActor
      )
    )

  case object ScheduleNewMixRequestTasks

  case object GetStatus
  case class Status(mixRequestAccountManager: MixRequestAccountManager)

}
