package com.gemini.jobcoin.actors

import akka.actor.{ActorRef, Props}
import com.gemini.jobcoin.accounting._

case class MockAPIAccessActor() extends MixerActor {

  import APIAccessActor._

  override def receive: Receive = handle(Ledger.emptyIdentifiableTransactionLedger)

  def handle(ledger: IdentifiableTransactionLedger): Receive = {
    case GetAllTransactions => sender() ! LatestLedger(ledger)
    case GetAddressInfo(address) =>
      sender() ! AddressInfo(ledger.addressInfo(address))
    case CommitTransaction(transaction) =>
      val sender: ActorRef = context.sender
      self ! StoredConfirmationWithOriginalSender(sender, StoredConfirmation(transaction))
      context.become(handle(ledger + transaction))
    case confirmation: StoredConfirmationWithOriginalSender =>
      confirmation.sender ! confirmation.storedConfirmation
  }
}

case class APIAccessActor() extends MixerActor {

  import APIAccessActor._

  override def receive: Receive = handle(Ledger.empty)

  def handle(ledger: BasicLedger): Receive = {
    case GetAllTransactions => ???
    case GetAddressInfo(address) => ???
    case CommitTransaction(transaction) => ???
    case confirmation: StoredConfirmationWithOriginalSender => confirmation.sender ! confirmation.storedConfirmation
  }
}

object MockAPIAccessActor {
  def props: Props = Props(MockAPIAccessActor())
}

object APIAccessActor {

  def props: Props = Props(APIAccessActor())

  case object GetAllTransactions

  case class GetAddressInfo(address: String)

  case class CommitTransaction(transaction: IdentifiableTransaction)

  case class LatestLedger(ledger: IdentifiableTransactionLedger)

  case class AddressInfo(account: Account)

  case class StoredConfirmation(transaction: IdentifiableTransaction)

  case class StoredConfirmationWithOriginalSender(sender: ActorRef, storedConfirmation: StoredConfirmation)

}