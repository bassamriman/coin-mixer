package com.gemini.jobcoin.mixrequest

import java.time.LocalDateTime

import com.gemini.jobcoin.accounting.MixingAccount

case class MixRequestAccountManager(mixingAccount: MixingAccount,
                                    mixRequestRegistry: MixRequestRegistry) {

  def registerNewMixRequest(
    mixRequest: Seq[MixRequest]
  ): MixRequestAccountManager =
    this.copy(mixRequestRegistry = mixRequestRegistry.register(mixRequest))

  def balanceReceivedFor(
    balanceMixRequestPairs: Seq[(BigDecimal, MixRequest)],
    timestamp: LocalDateTime
  ): (MixRequestAccountManager, Seq[MixRequestTask]) = {
    val (newMixRequestRegistry, mixRequestTasks) =
      mixRequestRegistry.balanceReceived(balanceMixRequestPairs, timestamp)
    this.copy(mixRequestRegistry = newMixRequestRegistry) -> mixRequestTasks
  }

  def balanceNotReceived(
    mixRequest: Seq[MixRequest],
    timestamp: LocalDateTime
  ): (MixRequestAccountManager, Seq[MixRequestTask]) = {
    val (newMixRequestRegistry, mixRequestTasks) =
      mixRequestRegistry.balanceNotReceived(mixRequest, timestamp)
    this.copy(mixRequestRegistry = newMixRequestRegistry) -> mixRequestTasks
  }

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

  def completeMixRequestTasks(
    mixRequestTasks: Seq[MixRequestTask],
    timestamp: LocalDateTime
  ): MixRequestAccountManager = {
    val (newMixRequestRegistry, _) =
      mixRequestRegistry.completeMixRequestTask(mixRequestTasks, timestamp)
    val newMixingAccount =
      mixingAccount.addCompletedMixRequestTasks(mixRequestTasks)
    this.copy(
      mixingAccount = newMixingAccount,
      mixRequestRegistry = newMixRequestRegistry
    )
  }

  def commitMixRequestTasks(
    mixRequestTasks: Seq[MixRequestTask],
    timestamp: LocalDateTime
  ): (MixRequestAccountManager, Seq[MixRequestTask]) = {
    val (newMixRequestRegistry, newMixRequestTasks) =
      mixRequestRegistry.commitMixRequestTask(mixRequestTasks, timestamp)
    (this.copy(mixRequestRegistry = newMixRequestRegistry), newMixRequestTasks)
  }

  def rollBackToScheduling(
    mixRequestTasks: Seq[MixRequestTask],
    timestamp: LocalDateTime
  ): (MixRequestAccountManager, Seq[MixRequestTask]) = {
    val (newMixRequestRegistry, newMixRequestTasks) =
      mixRequestRegistry.commitMixRequestTask(mixRequestTasks, timestamp)
    (this.copy(mixRequestRegistry = newMixRequestRegistry), newMixRequestTasks)
  }
}

object MixRequestAccountManager {
  def empty(address: String,
            mixingProperties: MixingProperties): MixRequestAccountManager =
    MixRequestAccountManager(
      MixingAccount.empty(address),
      MixRequestRegistry.empty(mixingProperties)
    )
}
