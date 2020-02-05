package com.gemini.jobcoin.mixrequest

import java.time.LocalDateTime

import com.gemini.jobcoin.accounting.MixingAccount

case class MixRequestAccountManager(mixingAccount: MixingAccount,
                                    mixRequestRegistry: MixRequestRegistry) {

  def registerNewMixRequest(mixRequest: Seq[MixRequest]): MixRequestAccountManager =
    this.copy(mixRequestRegistry = mixRequestRegistry.register(mixRequest))

  def balanceReceivedFor(balance: BigDecimal, mixRequest: MixRequest, timestamp: LocalDateTime)(seed: Long): MixRequestAccountManager =
    mixRequestRegistry.balanceReceived(balance, mixRequest, timestamp)(seed).map {
      newMixRequestRegistry => this.copy(mixRequestRegistry = newMixRequestRegistry)
    }.getOrElse(this)

  def balanceNotReceived(mixRequest: MixRequest, timestamp: LocalDateTime): MixRequestAccountManager =
    mixRequestRegistry.balanceNotReceived(mixRequest, timestamp).map {
      newMixRequestRegistry => this.copy(mixRequestRegistry = newMixRequestRegistry)
    }.getOrElse(this)

  def scheduleMixRequestTasks(timestamp: LocalDateTime)(seed: Long): (MixRequestAccountManager, Option[Seq[MixRequestTask]]) = {
    val (newMixRequestRegistry, mixRequestTasks) =
      mixRequestRegistry.scheduleMixRequestTasks(timestamp)(seed)
    if (mixingAccount.fundAvailableForMixRequestTasks(mixRequestTasks)) {
      val newMixingAccount = mixingAccount.addScheduledMixRequestTasks(mixRequestTasks)
      (this.copy(mixingAccount = newMixingAccount, mixRequestRegistry = newMixRequestRegistry), Some(mixRequestTasks))
    } else {
      (this, None)
    }
  }

  def completeMixRequestTasks(mixRequestTasks: Seq[MixRequestTask], timestamp: LocalDateTime): MixRequestAccountManager = {
    val (newMixRequestRegistry, _) = mixRequestRegistry.completeMixRequestTask(mixRequestTasks, timestamp)
    val newMixingAccount = mixingAccount.addCompletedMixRequestTasks(mixRequestTasks)
    this.copy(mixingAccount = newMixingAccount, mixRequestRegistry = newMixRequestRegistry)
  }


  def commitMixRequestTasks(mixRequestTasks: Seq[MixRequestTask], timestamp: LocalDateTime): (MixRequestAccountManager, Seq[MixRequestTask]) = {
    val (newMixRequestRegistry, newMixRequestTasks) = mixRequestRegistry.commitMixRequestTask(mixRequestTasks, timestamp)
    (this.copy(mixRequestRegistry = newMixRequestRegistry), newMixRequestTasks)
  }
}

object MixRequestAccountManager {
  def empty(address: String, mixingProperties: MixingProperties): MixRequestAccountManager =
    MixRequestAccountManager(MixingAccount.empty(address), MixRequestRegistry.empty(mixingProperties))
}
