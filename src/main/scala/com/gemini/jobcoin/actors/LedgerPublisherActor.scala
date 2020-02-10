package com.gemini.jobcoin.actors

import akka.actor.{ActorRef, Props}
import com.gemini.jobcoin.accounting.BasicLedger
import com.gemini.jobcoin.common.MixerActor

/**
  * LedgerPublisherActor is responsible for querying for the lastest Jobcoin ledger state
  * and publishing it to its subscribers
  * @param subscribers are the actors that will receive latest ledger updates
  * @param apiAccessActor will be queried to load latest Jobcoin ledger
  */
case class LedgerPublisherActor(subscribers: Seq[ActorRef],
                                apiAccessActor: ActorRef)
    extends MixerActor {

  import LedgerPublisherActor._

  override def receive: Receive = logged(handle)

  def handle: Receive = {
    case FetchLatestLedger => apiAccessActor ! APIAccessActor.GetAllTransactions
    case APIAccessActor.AllTransactionsLedger(newLedger) =>
      subscribers.foreach(_ ! LatestLedger(newLedger))
  }
}

object LedgerPublisherActor {
  def props(subscribers: Seq[ActorRef], apiAccessActor: ActorRef): Props =
    Props(LedgerPublisherActor(subscribers, apiAccessActor))

  case object SendMeLatestLedger

  case object FetchLatestLedger

  case class LatestLedger(ledger: BasicLedger)

}
