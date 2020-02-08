package com.gemini.jobcoin.actors

import akka.actor.Props
import com.gemini.jobcoin.JobcoinClient
import com.gemini.jobcoin.accounting._
import com.gemini.jobcoin.common.MixerActor

case class APIAccessActor(client: JobcoinClient) extends MixerActor {

  import APIAccessActor._
  import akka.pattern.pipe
  import context.dispatcher

  override def receive: Receive = handle

  def handle: Receive = {
    case GetAllTransactions =>
      client.getTransactions
        .map(transactions => AllTransactionsLedger(BasicLedger(transactions)))
        .pipeTo(self)(sender())
    case CommitTransaction(transaction) =>
      client
        .postTransaction(transaction.basicTransaction)
        .map {
          case Some(error) => CommitFailed(transaction, error)
          case None        => CommitSuccess(transaction)
        }
        .pipeTo(self)(sender())
    case response: AllTransactionsLedger =>
      sender() ! response
    case response: CommitSuccess =>
      sender() ! response
    case response: CommitFailed =>
      sender() ! response
  }
}

object APIAccessActor {

  def props(client: JobcoinClient): Props = Props(APIAccessActor(client))

  case object GetAllTransactions

  case class GetAddressInfo(address: String)

  case class CommitTransaction(transaction: IdentifiableTransaction)

  case class AllTransactionsLedger(ledger: BasicLedger)

  case class CommitFailed(transaction: IdentifiableTransaction, error: String)

  case class CommitSuccess(transaction: IdentifiableTransaction)
}
