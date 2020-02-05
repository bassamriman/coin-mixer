package com.gemini.jobcoin.actors

import java.time.LocalDateTime

import akka.actor.{ActorRef, Props}
import com.gemini.jobcoin.mixrequest.{MixRequest, MixRequestAccountManager, MixRequestTask, MixingProperties}

import scala.util.Random

case class AccountManagerActor(address: String,
                               mixingProperties: MixingProperties) extends MixerActor {

  import AccountManagerActor._

  override def receive: Receive = idle

  def idle: Receive = {
    case StartWith(balanceMonitorActor: ActorRef, committerActor: ActorRef, validatorActor: ActorRef) =>
      val mixRequestAccountManager = MixRequestAccountManager.empty(address, mixingProperties)
      val seed: Long = Random.nextLong()
      context.become(
        handle(
          mixRequestAccountManager = mixRequestAccountManager,
          seed = seed,
          balanceMonitorActor = balanceMonitorActor,
          committerActor = committerActor,
          validatorActor = validatorActor))
  }

  def handle(mixRequestAccountManager: MixRequestAccountManager,
             seed: Long,
             balanceMonitorActor: ActorRef,
             committerActor: ActorRef,
             validatorActor: ActorRef): Receive = {
    case NewRequest(mixRequests) =>
      balanceMonitorActor ! BalanceMonitorActor.NewRequestsAwaitingBalance(mixRequests)
      val newMixRequestAccountManager = mixRequestAccountManager.registerNewMixRequest(mixRequests)
      context.become(
        handle(
          mixRequestAccountManager = newMixRequestAccountManager,
          seed = seed,
          balanceMonitorActor = balanceMonitorActor,
          committerActor = committerActor,
          validatorActor = validatorActor))

    case BalanceReceived(mixRequestsToBalance) =>
      val now = LocalDateTime.now()
      val newMixRequestAccountManager =
        mixRequestsToBalance.foldLeft(mixRequestAccountManager)((mixRequestAccountManager, entry) => mixRequestAccountManager.balanceReceivedFor(entry._2, entry._1, now)(seed))
      context.become(
        handle(
          mixRequestAccountManager = newMixRequestAccountManager,
          seed = seed + 1,
          balanceMonitorActor = balanceMonitorActor,
          committerActor = committerActor,
          validatorActor = validatorActor))

    case BalanceNotReceived(mixRequest) =>
      val newMixRequestAccountManager = mixRequestAccountManager.balanceNotReceived(mixRequest, LocalDateTime.now())
      context.become(
        handle(
          mixRequestAccountManager = newMixRequestAccountManager,
          seed = seed,
          balanceMonitorActor = balanceMonitorActor,
          committerActor = committerActor,
          validatorActor = validatorActor))

    case ScheduleNewMixRequestTasks =>
      val (newMixRequestAccountManager, maybeScheduledMixRequestTasks) =
        mixRequestAccountManager.scheduleMixRequestTasks(LocalDateTime.now())(seed)
      maybeScheduledMixRequestTasks
        .foreach(scheduledMixRequestTasks =>
          scheduledMixRequestTasks.foreach(committerActor ! CommitterActor.Commit(_))
        )
      context.become(
        handle(
          mixRequestAccountManager = newMixRequestAccountManager,
          seed = seed + 1,
          balanceMonitorActor = balanceMonitorActor,
          committerActor = committerActor,
          validatorActor = validatorActor))

    case Committed(mixRequestTask) =>
      val (newMixRequestAccountManager, scheduledMixRequestTasks) =
        mixRequestAccountManager.commitMixRequestTasks(mixRequestTask, LocalDateTime.now())
      validatorActor ! ValidatorActor.Validate(scheduledMixRequestTasks)
      context.become(
        handle(
          mixRequestAccountManager = newMixRequestAccountManager,
          seed = seed,
          balanceMonitorActor = balanceMonitorActor,
          committerActor = committerActor,
          validatorActor = validatorActor))

    case Complete(mixRequestTask) =>
      val newMixRequestAccountManager =
        mixRequestAccountManager.completeMixRequestTasks(mixRequestTask, LocalDateTime.now())
      context.become(
        handle(
          mixRequestAccountManager = newMixRequestAccountManager,
          seed = seed,
          balanceMonitorActor = balanceMonitorActor,
          committerActor = committerActor,
          validatorActor = validatorActor))
  }

}

object AccountManagerActor {
  def props(address: String,
            mixingProperties: MixingProperties): Props =
    Props(AccountManagerActor(address = address, mixingProperties = mixingProperties))

  case class StartWith(balanceMonitorActor: ActorRef,
                       committerActor: ActorRef,
                       validatorActor: ActorRef)

  case class NewRequest(mixRequest: Seq[MixRequest])

  case class BalanceReceived(mixRequestsToBalance: Seq[(MixRequest, BigDecimal)])

  case class BalanceNotReceived(mixRequest: MixRequest)

  case object ScheduleNewMixRequestTasks

  case class Committed(mixRequestTasks: Seq[MixRequestTask])

  case class Complete(mixRequestTasks: Seq[MixRequestTask])

}

