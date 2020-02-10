package com.gemini.jobcoin.actors

import java.time.LocalDateTime

import akka.actor.{ActorRef, Props}
import com.gemini.jobcoin.accounting.{
  IdentifiableTransaction,
  TransactionGenerator
}
import com.gemini.jobcoin.common.MixerActor
import com.gemini.jobcoin.mixrequest.models.{
  MixRequestWithBalance,
  MixingProperties
}

/**
  * MixedTransactionGeneratorActor is responsible for generating random transactions
  * from source address to destination addresses.
  * It will return a list of transactions that is the result of mixing the money from
  * source address in the destination addresses.
  * @param mixingProperties is the parameters we can tune to better optimise performance
  * @param initialSeed is the Seed used to control random number generation
  *                    (good for reproducing issues).
  */
case class MixedTransactionGeneratorActor(mixingProperties: MixingProperties,
                                          initialSeed: Long)
    extends MixerActor {

  import MixedTransactionGeneratorActor._

  override def receive: Receive = logged(handle(initialSeed))

  def handle(seed: Long): Receive = {
    case GenerateMixedTransactions(mixingAddress, mixRequests) =>
      val sender: ActorRef = context.sender()
      val result: Seq[(MixRequestWithBalance, Seq[IdentifiableTransaction])] =
        mixRequests.map(mixRequest => {
          val transactions: Seq[IdentifiableTransaction] =
            TransactionGenerator.generateTransactions(
              amountToDistribute = mixRequest.sourceAddressBalance,
              sourceAddress = mixingAddress,
              destinationAddresses = mixRequest.destinationAddresses,
              minTransactionAmount = mixingProperties.minTransactionAmount,
              maxTransactionAmount = mixingProperties.maxTransactionAmount,
              maxScale = mixingProperties.maxScale
            )(seed)
          (mixRequest, transactions)
        })
      sender ! MixedTransactions(result, LocalDateTime.now())
      context.become(logged(handle(seed + 1)))
  }
}

object MixedTransactionGeneratorActor {
  def props(mixingProperties: MixingProperties, initialSeed: Long): Props =
    Props(MixedTransactionGeneratorActor(mixingProperties, initialSeed))

  case class GenerateMixedTransactions(mixingAddress: String,
                                       mixRequests: Seq[MixRequestWithBalance])

  case class MixedTransactions(
    mixRequestTransactionsPairs: Seq[
      (MixRequestWithBalance, Seq[IdentifiableTransaction])
    ],
    timestamp: LocalDateTime
  )

}
