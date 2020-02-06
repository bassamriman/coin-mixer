package com.gemini.jobcoin.actors

import akka.actor.{ActorRef, Props}
import com.gemini.jobcoin.accounting.{IdentifiableTransaction, TransactionGenerator}
import com.gemini.jobcoin.common.MixerActor
import com.gemini.jobcoin.mixrequest.{MixRequestWithBalance, MixingProperties}

case class MixedTransactionGeneratorActor(mixingProperties: MixingProperties, initialSeed: Long) extends MixerActor {

  import MixedTransactionGeneratorActor._

  override def receive: Receive = handle(initialSeed)

  def handle(seed: Long): Receive = {
    case GenerateMixedTransactions(mixRequests) =>
      val sender: ActorRef = context.sender()
      val result: Seq[(MixRequestWithBalance, Seq[IdentifiableTransaction])] =
        mixRequests.map(mixRequest => {
          val transactions: Seq[IdentifiableTransaction] = TransactionGenerator.generateTransactions(
            amountToDistribute = mixRequest.sourceAddressBalance,
            sourceAddress = mixRequest.sourceAddress,
            destinationAddresses = mixRequest.destinationAddresses,
            minTransactionPerDestinationAddress = mixingProperties.minTransactionPerDestinationAddress,
            maxTransactionPerDestinationAddress = mixingProperties.maxTransactionPerDestinationAddress,
            minTransactionAmount = mixingProperties.minTransactionAmount,
            maxTransactionAmount = mixingProperties.maxTransactionAmount,
            maxScale = mixingProperties.maxScale
          )(seed)
          (mixRequest, transactions)
        })
      sender ! MixedTransactions(result)
      context.become(handle(seed + 1))
  }
}

object MixedTransactionGeneratorActor {
  def props(mixingProperties: MixingProperties, initialSeed: Long): Props =
    Props(MixedTransactionGeneratorActor(mixingProperties, initialSeed))

  case class GenerateMixedTransactions(mixRequests: Seq[MixRequestWithBalance])

  case class MixedTransactions(mixRequestTransactionsPairs: Seq[(MixRequestWithBalance, Seq[IdentifiableTransaction])])

}
