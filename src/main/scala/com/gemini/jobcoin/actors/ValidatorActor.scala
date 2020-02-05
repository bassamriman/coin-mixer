package com.gemini.jobcoin.actors

import akka.actor.{ActorRef, Props}
import com.gemini.jobcoin.mixrequest.MixRequestTask

case class ValidatorActor() extends MixerActor {

  import ValidatorActor._

  override def receive: Receive = idle

  def idle: Receive = {
    case StartWith(accountManagerActorRef) =>
      context.become(handle(Map.empty, accountManagerActorRef))
  }

  def handle(mixRequestTaskToValidate: Map[String, MixRequestTask], accountManagerActorRef: ActorRef): Receive = {
    case Validate(mixRequestTasks) =>
      val newMixRequestTaskToValidate = mixRequestTaskToValidate ++ mixRequestTasks.map(mixRequestTask => mixRequestTask.id -> mixRequestTask)
      context.become(
        handle(
          mixRequestTaskToValidate = newMixRequestTaskToValidate,
          accountManagerActorRef = accountManagerActorRef))

    case LedgerActor.LatestLedger(newLedger) =>
      //TODO: Make this more efficient. Loop through all transactions once for multiple mix request
      val presentMixRequestTaskIds: Seq[String] = mixRequestTaskToValidate
        .mapValues(mixRequestTask => newLedger.exist(mixRequestTask.transaction))
        .filter(_._2).keys.toSeq
      val presentMixRequestTasks: Seq[MixRequestTask] = presentMixRequestTaskIds.flatMap(presentMixRequestTaskId => mixRequestTaskToValidate.get(presentMixRequestTaskId))
      val newMixRequestTaskToValidate = mixRequestTaskToValidate -- presentMixRequestTaskIds
      accountManagerActorRef ! AccountManagerActor.Complete(presentMixRequestTasks)
      context.become(handle(
        mixRequestTaskToValidate = newMixRequestTaskToValidate,
        accountManagerActorRef = accountManagerActorRef))
  }

}

object ValidatorActor {
  def props: Props = Props(ValidatorActor())

  case class StartWith(accountManagerActorRef: ActorRef)

  case class Validate(mixRequestTasks: Seq[MixRequestTask])

}