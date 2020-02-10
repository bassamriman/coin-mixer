package com.gemini.jobcoin.mixrequest

import java.time.LocalDateTime

import com.gemini.jobcoin.accounting.{IdentifiableTransaction, MixingAccount}
import com.gemini.jobcoin.mixrequest.models.{
  MixRequest,
  MixRequestWithBalance,
  MixingProperties
}

/**
  * MixRequestAccountManager controls both the mixingAccount and mixRequestRegistry
  * based on the wold events.
  * For instance making sure there is funds before allowing transactions to be persisted.
  *
  * @param mixingAccount The mixing account that managed state of the account.
  * @param mixRequestRegistry Holds all mixing requests and their states.
  */
case class MixRequestAccountManager(mixingAccount: MixingAccount,
                                    mixRequestRegistry: MixRequestRegistry) {

  /**
    * Register new mix Requests
    * @param mixRequest
    * @return new MixRequestAccountManager state
    */
  def registerNewMixRequest(
    mixRequest: Seq[MixRequest]
  ): MixRequestAccountManager =
    this.copy(mixRequestRegistry = mixRequestRegistry.register(mixRequest))

  /**
    * Mark that the balance was received by the user for the given mix requests.
    * @param balanceMixRequestPairs balance to mix request
    * @param timestamp time that the balance was received
    * @return
    */
  def balanceReceivedFor(
    balanceMixRequestPairs: Seq[(BigDecimal, MixRequest)],
    timestamp: LocalDateTime
  ): (MixRequestAccountManager, Seq[MixRequestTask]) = {
    val (newMixRequestRegistry, mixRequestTasks) =
      mixRequestRegistry.balanceReceived(balanceMixRequestPairs, timestamp)
    this.copy(mixRequestRegistry = newMixRequestRegistry) -> mixRequestTasks
  }

  /**
    * Cancel mix request for not receiving the balance
    * (NOT USED: tracking time of a mix request is not yet implemented)
    * @param mixRequest
    * @param timestamp
    * @return
    */
  def balanceNotReceived(
    mixRequest: Seq[MixRequest],
    timestamp: LocalDateTime
  ): (MixRequestAccountManager, Seq[MixRequestTask]) = {
    val (newMixRequestRegistry, mixRequestTasks) =
      mixRequestRegistry.balanceNotReceived(mixRequest, timestamp)
    this.copy(mixRequestRegistry = newMixRequestRegistry) -> mixRequestTasks
  }

  /**
    * Adds generated random transactions to the mix requests and creates their
    * corresponding mix request tasks.
    * @param mixRequestTransactionsPairs mix request with corresponding transactions
    * @param timestamp
    * @return
    */
  def startMixing(
    mixRequestTransactionsPairs: Seq[
      (MixRequestWithBalance, Seq[IdentifiableTransaction])
    ],
    timestamp: LocalDateTime
  ): (MixRequestAccountManager, Seq[MixRequestTask]) = {
    val (newMixRequestRegistry, mixRequestTasks) =
      mixRequestRegistry.startMixing(mixRequestTransactionsPairs, timestamp)
    this.copy(mixRequestRegistry = newMixRequestRegistry) -> mixRequestTasks
  }

  /**
    * Elects mix request tasks to be persisted
    * @param timestamp
    * @param seed
    * @return
    */
  def scheduleMixRequestTasks(
    timestamp: LocalDateTime
  )(seed: Long): (MixRequestAccountManager, Seq[MixRequestTask]) = {
    val (newMixRequestRegistry, mixRequestTasks) =
      mixRequestRegistry.scheduleMixRequestTasks(timestamp)(seed)
    if (mixingAccount.fundAvailableForMixRequestTasks(mixRequestTasks)) {
      val newMixingAccount =
        mixingAccount.addScheduledMixRequestTasks(mixRequestTasks)
      (
        this.copy(
          mixingAccount = newMixingAccount,
          mixRequestRegistry = newMixRequestRegistry
        ),
        mixRequestTasks
      )
    } else {
      (this, Seq.empty)
    }
  }

  /**
    * Updated the state of given mix request tasks to completed
    * @param mixRequestTasks
    * @param timestamp
    * @return
    */
  def completeMixRequestTasks(
    mixRequestTasks: Seq[MixRequestTask],
    timestamp: LocalDateTime
  ): MixRequestAccountManager = {
    val (newMixRequestRegistry, _) =
      mixRequestRegistry.completeMixRequestTasks(mixRequestTasks, timestamp)
    val newMixingAccount =
      mixingAccount.addCompletedMixRequestTasks(mixRequestTasks)
    this.copy(
      mixingAccount = newMixingAccount,
      mixRequestRegistry = newMixRequestRegistry
    )
  }

  /**
    * Updated the state of the given mix request tasks to committed
    * @param mixRequestTasks
    * @param timestamp
    * @return
    */
  def commitMixRequestTasks(
    mixRequestTasks: Seq[MixRequestTask],
    timestamp: LocalDateTime
  ): (MixRequestAccountManager, Seq[MixRequestTask]) = {
    val (newMixRequestRegistry, newMixRequestTasks) =
      mixRequestRegistry.commitMixRequestTasks(mixRequestTasks, timestamp)
    (this.copy(mixRequestRegistry = newMixRequestRegistry), newMixRequestTasks)
  }

  /**
    * Reset state of the given mix request tasks. Used in case of failures.
    * @param mixRequestTasks
    * @param timestamp
    * @return
    */
  def rollBackToScheduling(
    mixRequestTasks: Seq[MixRequestTask],
    timestamp: LocalDateTime
  ): (MixRequestAccountManager, Seq[MixRequestTask]) = {
    val (newMixRequestRegistry, newMixRequestTasks) =
      mixRequestRegistry.commitMixRequestTasks(mixRequestTasks, timestamp)
    (this.copy(mixRequestRegistry = newMixRequestRegistry), newMixRequestTasks)
  }
}

object MixRequestAccountManager {
  def empty(address: String,
            mixingProperties: MixingProperties,
            numberOfMixRequestTaskToSchedule: Int): MixRequestAccountManager =
    MixRequestAccountManager(
      MixingAccount.empty(address),
      MixRequestRegistry
        .empty(mixingProperties, numberOfMixRequestTaskToSchedule)
    )
}
