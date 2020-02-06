package com.gemini.jobcoin.actors

import akka.actor.{ActorRef, Props}
import com.gemini.jobcoin.accounting.IdentifiableTransactionLedger
import com.gemini.jobcoin.common.MixerActor

case class LedgerActor(subscribers: Seq[ActorRef], apiAccessActor: ActorRef) extends MixerActor {

  import LedgerActor._

  override def receive: Receive = handle

  def handle: Receive = {
    case FetchLatestLedger => apiAccessActor ! APIAccessActor.GetAllTransactions
    case APIAccessActor.LatestLedger(newLedger) =>
      subscribers.foreach(_ ! LatestLedger(newLedger))
  }
}

object LedgerActor {
  def props(subscribers: Seq[ActorRef], apiAccessActor: ActorRef): Props =
    Props(LedgerActor(subscribers, apiAccessActor))

  case object SendMeLatestLedger

  case object FetchLatestLedger

  case class LatestLedger(ledger: IdentifiableTransactionLedger)

}
