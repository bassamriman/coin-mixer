package com.gemini.jobcoin

import java.time.LocalDateTime

import com.gemini.jobcoin.mixrequest.{MixRequest, MixRequestRegistry, MixRequestTask}

case class MixRequestAccountManager(mixingAccount: MixingAccount,
                                    mixRequestRegistry: MixRequestRegistry,
                                    numberOfMixRequestTaskToSchedule: Int) {

  def registerNewMixRequest(mixRequest: Seq[MixRequest]): MixRequestAccountManager =
    this.copy(mixRequestRegistry = mixRequestRegistry.register(mixRequest))

  def balanceReceivedFor(balance: BigDecimal, mixRequest: MixRequest, timestamp: LocalDateTime): MixRequestAccountManager =
    mixRequestRegistry.balanceReceived(balance, mixRequest, timestamp).map {
      newMixRequestRegistry => this.copy(mixRequestRegistry = newMixRequestRegistry)
    }.getOrElse(this)

  def balanceNotReceived(mixRequest: MixRequest, timestamp: LocalDateTime): MixRequestAccountManager = ???

  def scheduleMixRequestTasks(timestamp: LocalDateTime)(seed: Long): (MixRequestAccountManager, Option[Seq[MixRequestTask]]) = {
    val (newMixRequestRegistry, mixRequestTasks) =
      mixRequestRegistry.scheduleMixRequestTasks(numberOfMixRequestTaskToSchedule, timestamp)(seed)
    if (mixingAccount.fundAvailableForMixRequestTasks(mixRequestTasks)) {
      val newMixingAccount = mixingAccount.addScheduledMixRequestTasks(mixRequestTasks)
      (this.copy(mixingAccount = newMixingAccount, mixRequestRegistry = newMixRequestRegistry), Some(mixRequestTasks))
    } else {
      (this, None)
    }
  }

  def completeMixRequestTasks(mixRequestTasks: Seq[MixRequestTask]): MixRequestAccountManager = ???

  def commitMixRequestTasks(mixRequestTasks: Seq[MixRequestTask]): MixRequestAccountManager = ???
}
