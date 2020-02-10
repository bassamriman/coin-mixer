package com.gemini.jobcoin.actors

import java.time.LocalDateTime

import akka.actor.{ActorRef, Props}
import com.gemini.jobcoin.common.MixerActor
import com.gemini.jobcoin.mixrequest.models.MixRequest

/**
  * Actor that is responsible for detecting when money is sent to a specific address
  */
case class BalanceMonitorActor() extends MixerActor {

  import BalanceMonitorActor._

  override def receive: Receive = logged(handle(Map.empty, Map.empty))

  def handle(requestsAwaitingBalance: Map[String, MixRequest],
             mixRequestIdToSender: Map[String, ActorRef]): Receive = {
    case NewRequestsAwaitingBalance(mixRequests) =>
      val sender = context.sender()
      val newRequestAwaitingBalance = requestsAwaitingBalance ++ mixRequests
        .map(mixRequest => mixRequest.id -> mixRequest)
      val newMixRequestIdToSender = mixRequestIdToSender ++ mixRequests.map(
        mixRequest => mixRequest.id -> sender
      )
      context.become(
        logged(
          handle(
            requestsAwaitingBalance = newRequestAwaitingBalance,
            mixRequestIdToSender = newMixRequestIdToSender
          )
        )
      )
    case LedgerPublisherActor.LatestLedger(newLedger) =>
      //TODO: Make this more efficient. Loop through all transactions once for multiple address
      val mixRequestIdToBalance: Map[String, BigDecimal] =
        requestsAwaitingBalance
          .mapValues(
            mixRequest => newLedger.receivedAmount(mixRequest.sourceAddress)
          )
          .filter(_._2 != 0)
      val mixRequestsBalancePairs: Seq[(MixRequest, BigDecimal)] =
        mixRequestIdToBalance.toSeq.flatMap(
          entry => requestsAwaitingBalance.get(entry._1).map(_ -> entry._2)
        )
      val newRequestAwaitingBalance = requestsAwaitingBalance -- mixRequestIdToBalance.keys
      val newMixRequestIdToSender = mixRequestIdToSender -- mixRequestIdToBalance.keys

      val groupedMixRequestsBalancePairs
        : Map[ActorRef, Seq[(MixRequest, BigDecimal)]] =
        mixRequestsBalancePairs.groupBy(
          mixRequestBalancePair =>
            mixRequestIdToSender(mixRequestBalancePair._1.id)
        )

      val now = LocalDateTime.now()
      groupedMixRequestsBalancePairs.foreach {
        senderToMixRequestsBalancePairs =>
          val (sender, mixRequestsBalancePairs) =
            senderToMixRequestsBalancePairs
          sender ! BalanceReceived(mixRequestsBalancePairs.map(_.swap), now)
      }

      context.become(
        logged(
          handle(
            requestsAwaitingBalance = newRequestAwaitingBalance,
            mixRequestIdToSender = newMixRequestIdToSender
          )
        )
      )
  }

}

object BalanceMonitorActor {
  def props: Props = Props(BalanceMonitorActor())

  case class NewRequestsAwaitingBalance(mixRequest: Seq[MixRequest])

  case class BalanceReceived(
    balanceMixRequestPairs: Seq[(BigDecimal, MixRequest)],
    timestamp: LocalDateTime
  )

  case class BalanceNotReceived(mixRequests: Seq[MixRequest],
                                timestamp: LocalDateTime)

}
