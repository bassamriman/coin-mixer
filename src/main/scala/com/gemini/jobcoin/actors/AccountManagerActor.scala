package com.gemini.jobcoin.actors

import java.time.LocalDateTime

import akka.actor.{ActorRef, Props}
import com.gemini.jobcoin.common.MixerActor
import com.gemini.jobcoin.mixrequest.{
  MixRequest,
  MixRequestAccountManager,
  MixingProperties
}

case class AccountManagerActor(address: String,
                               mixingProperties: MixingProperties,
                               initialSeed: Long,
                               balanceMonitorActor: ActorRef,
                               mixedTransactionGeneratorActor: ActorRef,
                               committerActor: ActorRef,
                               validatorActor: ActorRef)
    extends MixerActor {

  import AccountManagerActor._

  override def receive: Receive =
    handle(
      MixRequestAccountManager.empty(address, mixingProperties),
      initialSeed
    )

  def handle(mixRequestAccountManager: MixRequestAccountManager,
             seed: Long): Receive = {
    case NewRequestDispatcherActor
          .NewRequests(mixRequestCoordinates, timestamp) =>
      val newMixRequests = mixRequestCoordinates.map(
        mixRequestCoordinate =>
          MixRequest(address, timestamp, mixRequestCoordinate)
      )
      balanceMonitorActor ! BalanceMonitorActor.NewRequestsAwaitingBalance(
        newMixRequests
      )
      val newMixRequestAccountManager =
        mixRequestAccountManager.registerNewMixRequest(newMixRequests)
      context.become(
        handle(
          mixRequestAccountManager = newMixRequestAccountManager,
          seed = seed
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
        handle(
          mixRequestAccountManager = newMixRequestAccountManager,
          seed = seed
        )
      )

    case BalanceMonitorActor.BalanceNotReceived(mixRequests, timestamp) =>
      val (newMixRequestAccountManager, _) =
        mixRequestAccountManager.balanceNotReceived(mixRequests, timestamp)
      context.become(
        handle(
          mixRequestAccountManager = newMixRequestAccountManager,
          seed = seed
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
        handle(
          mixRequestAccountManager = newMixRequestAccountManager,
          seed = seed + 1
        )
      )

    case CommitterActor.Committed(mixRequestTask) =>
      val (newMixRequestAccountManager, scheduledMixRequestTasks) =
        mixRequestAccountManager.commitMixRequestTasks(
          mixRequestTask,
          LocalDateTime.now()
        )
      validatorActor ! ValidatorActor.Validate(scheduledMixRequestTasks)
      context.become(
        handle(
          mixRequestAccountManager = newMixRequestAccountManager,
          seed = seed
        )
      )

    case CommitterActor.FailedToCommit(mixRequestTask) =>
      val (newMixRequestAccountManager, _) =
        mixRequestAccountManager.rollBackToScheduling(
          mixRequestTask,
          LocalDateTime.now()
        )
      context.become(
        handle(
          mixRequestAccountManager = newMixRequestAccountManager,
          seed = seed
        )
      )

    case ValidatorActor.Validated(mixRequestTask) =>
      val newMixRequestAccountManager =
        mixRequestAccountManager.completeMixRequestTasks(
          mixRequestTask,
          LocalDateTime.now()
        )
      context.become(
        handle(
          mixRequestAccountManager = newMixRequestAccountManager,
          seed = seed
        )
      )
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
        address = address,
        mixingProperties = mixingProperties,
        initialSeed = initialSeed,
        balanceMonitorActor = balanceMonitorActor,
        mixedTransactionGeneratorActor = mixedTransactionGeneratorActor,
        committerActor = committerActor,
        validatorActor = validatorActor
      )
    )

  case object ScheduleNewMixRequestTasks

}
