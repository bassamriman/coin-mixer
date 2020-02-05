package com.gemini.jobcoin.actors

import akka.actor.{ActorRef, Props}
import com.gemini.jobcoin.mixrequest.MixRequest

case class BalanceMonitorActor() extends MixerActor {

  import BalanceMonitorActor._

  override def receive: Receive = idle

  def idle: Receive = {
    case StartWith(accountManagerActorRef) =>
      context.become(handle(Map.empty, accountManagerActorRef))
  }

  def handle(requestAwaitingBalance: Map[String, MixRequest], accountManagerActorRef: ActorRef): Receive = {
    case NewRequestsAwaitingBalance(mixRequests) =>
      val newRequestAwaitingBalance = requestAwaitingBalance ++ mixRequests.map(mixRequest => mixRequest.id -> mixRequest)
      context.become(
        handle(
          requestAwaitingBalance = newRequestAwaitingBalance,
          accountManagerActorRef = accountManagerActorRef))
    case LedgerActor.LatestLedger(newLedger) =>
      //TODO: Make this more efficient. Loop through all transactions once for multiple address
      val mixRequestIdToBalance: Map[String, BigDecimal] = requestAwaitingBalance
        .mapValues(mixRequest => newLedger.receivedAmount(mixRequest.sourceAddress))
        .filter(_._2 == 0)
      val mixRequestsToBalance: Seq[(MixRequest, BigDecimal)] = mixRequestIdToBalance.toSeq.flatMap(entry => requestAwaitingBalance.get(entry._1).map(_ -> entry._2))
      val newRequestAwaitingBalance = requestAwaitingBalance -- mixRequestIdToBalance.keys
      accountManagerActorRef ! AccountManagerActor.BalanceReceived(mixRequestsToBalance)
      context.become(handle(newRequestAwaitingBalance, accountManagerActorRef))
  }

}

object BalanceMonitorActor {
  def props: Props = Props(BalanceMonitorActor())

  case class StartWith(accountManagerActorRef: ActorRef)

  case class NewRequestsAwaitingBalance(mixRequest: Seq[MixRequest])

}