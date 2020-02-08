package com.gemini.jobcoin.actors

import akka.actor.{ActorRef, Props}
import com.gemini.jobcoin.common.MixerActor
import com.gemini.jobcoin.mixrequest.MixRequestTask

case class CommitterActor(apiAccessActor: ActorRef) extends MixerActor {

  import CommitterActor._

  override def receive: Receive = handle(Map.empty, Map.empty)

  def handle(transactionAwaitingConfirmation: Map[String, MixRequestTask],
             mixRequestTaskToSender: Map[String, ActorRef]): Receive = {
    case Commit(mixRequestTask) =>
      val sender = context.sender()
      apiAccessActor ! APIAccessActor.CommitTransaction(
        mixRequestTask.transaction
      )
      val newTransactionAwaitingConfirmation: Map[String, MixRequestTask] =
        transactionAwaitingConfirmation + (mixRequestTask.transaction.id -> mixRequestTask)
      val newMixRequestTaskToSender: Map[String, ActorRef] =
        mixRequestTaskToSender + (mixRequestTask.transaction.id -> sender)
      context.become(
        handle(
          transactionAwaitingConfirmation = newTransactionAwaitingConfirmation,
          mixRequestTaskToSender = newMixRequestTaskToSender
        )
      )

    case APIAccessActor.CommitSuccess(transaction) =>
      val correspondingMixRequestTask: MixRequestTask =
        transactionAwaitingConfirmation(transaction.id)
      val newTransactionAwaitingConfirmation: Map[String, MixRequestTask] =
        transactionAwaitingConfirmation - transaction.id
      val newMixRequestTaskToSender: Map[String, ActorRef] =
        mixRequestTaskToSender - correspondingMixRequestTask.id

      val sender = mixRequestTaskToSender(correspondingMixRequestTask.id)

      sender ! Committed(Seq(correspondingMixRequestTask))

      context.become(
        handle(
          transactionAwaitingConfirmation = newTransactionAwaitingConfirmation,
          mixRequestTaskToSender = newMixRequestTaskToSender
        )
      )
    case APIAccessActor.CommitFailed(transaction, error) =>
      log.error(s"Failed to commit with following: $error")
      val correspondingMixRequestTask: MixRequestTask =
        transactionAwaitingConfirmation(transaction.id)
      val newTransactionAwaitingConfirmation: Map[String, MixRequestTask] =
        transactionAwaitingConfirmation - transaction.id
      val newMixRequestTaskToSender: Map[String, ActorRef] =
        mixRequestTaskToSender - correspondingMixRequestTask.id

      val sender = mixRequestTaskToSender(correspondingMixRequestTask.id)

      sender ! FailedToCommit(Seq(correspondingMixRequestTask))

      context.become(
        handle(
          transactionAwaitingConfirmation = newTransactionAwaitingConfirmation,
          mixRequestTaskToSender = newMixRequestTaskToSender
        )
      )

  }
}

object CommitterActor {
  def props(apiAccessActor: ActorRef): Props =
    Props(CommitterActor(apiAccessActor))

  case class Commit(mixRequestTask: MixRequestTask)

  case class Committed(mixRequestTasks: Seq[MixRequestTask])
  case class FailedToCommit(mixRequestTasks: Seq[MixRequestTask])

}
