package com.gemini.jobcoin.actors

import akka.actor.{ActorRef, Props}
import com.gemini.jobcoin.accounting.IdentifiableTransactionLedger

case class LedgerActor(subscribers: Seq[ActorRef]) extends MixerActor {

  import LedgerActor._

  override def receive: Receive = idle

  def idle: Receive = {
    case StartWith(apiAccessActor) =>
      context.become(handle(apiAccessActor))
  }

  def handle(apiAccessActor: ActorRef): Receive = {
    case FetchLatestLedger => apiAccessActor ! APIAccessActor.GetAllTransactions
    case APIAccessActor.LatestLedger(newLedger) =>
      subscribers.foreach(_ ! LatestLedger(newLedger))
      context.become(handle(apiAccessActor))
  }
}

object LedgerActor {
  def props(subscribers: Seq[ActorRef]): Props = Props(LedgerActor(subscribers))

  case class StartWith(apiAccessActor: ActorRef)

  case object SendMeLatestLedger

  case object FetchLatestLedger

  case class LatestLedger(ledger: IdentifiableTransactionLedger)

}
