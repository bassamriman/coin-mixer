package com.gemini.jobcoin.actors

import akka.actor.{ActorRef, Props}
import com.gemini.jobcoin.mixrequest.MixRequestTask

case class CommitterActor() extends MixerActor {

  import CommitterActor._

  override def receive: Receive = idle

  def idle: Receive = {
    case StartWith(apiAccessActor, accountManagerActorRef) =>
      context.become(handle(Map.empty, apiAccessActor, accountManagerActorRef))
  }

  def handle(transactionAwaitingConfirmation: Map[String, MixRequestTask],
             apiAccessActor: ActorRef,
             accountManagerActorRef: ActorRef): Receive = {
    case Commit(mixRequestTask) =>
      apiAccessActor ! APIAccessActor.CommitTransaction(mixRequestTask.transaction)
      val newTransactionAwaitingConfirmation =
        transactionAwaitingConfirmation + (mixRequestTask.transaction.id -> mixRequestTask)
      context.become(
        handle(
          transactionAwaitingConfirmation = newTransactionAwaitingConfirmation,
          apiAccessActor = apiAccessActor,
          accountManagerActorRef = accountManagerActorRef))

    case APIAccessActor.StoredConfirmation(transaction) =>
      val newTransactionAwaitingConfirmation =
        transactionAwaitingConfirmation - transaction.id
      transactionAwaitingConfirmation
        .get(transaction.id)
        .foreach(mixRequestTask =>
          accountManagerActorRef ! AccountManagerActor.Committed(Seq(mixRequestTask)))
      context.become(
        handle(
          transactionAwaitingConfirmation = newTransactionAwaitingConfirmation,
          apiAccessActor = apiAccessActor,
          accountManagerActorRef = accountManagerActorRef))

  }
}

object CommitterActor {
  def props: Props = Props(CommitterActor())

  case class StartWith(apiAccessActor: ActorRef, accountManagerActorRef: ActorRef)

  case class Commit(mixRequestTask: MixRequestTask)

}
