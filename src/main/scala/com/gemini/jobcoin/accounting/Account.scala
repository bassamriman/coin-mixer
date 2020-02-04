package com.gemini.jobcoin

import com.gemini.jobcoin.mixrequest.MixRequestTask

trait Account {
  val address: String
  val startingBalance: BigDecimal
  val currentBalance: BigDecimal
  val ledger: Map[String, Transaction]
}

case class StandardAccount(address: String, startingBalance: BigDecimal, ledger: Map[String, Transaction]) extends Account {
  val currentBalance: BigDecimal = startingBalance + ledger.map(_._2.signAdjustAmount(address)).sum
}

case class ReservableBalanceAccount(address: String,
                                    account: Account,
                                    reservedBalanceAccount: Account) extends Account {
  val startingBalance: BigDecimal = account.startingBalance - reservedBalanceAccount.startingBalance
  val currentBalance: BigDecimal = account.currentBalance - reservedBalanceAccount.currentBalance
  val reservedBalance: BigDecimal = reservedBalanceAccount.currentBalance

  val ledger: Map[String, Transaction] = account.ledger ++ reservedBalanceAccount.ledger

  def allocatedAmount(transactions: Seq[Transaction]): ReservableBalanceAccount = ???

  def commitTransactions(transactions: Seq[Transaction]): ReservableBalanceAccount = ???

}


case class MixingAccount(reservedBalanceAccount: ReservableBalanceAccount,
                         scheduledMixRequestTask: Map[String, MixRequestTask],
                         completeMixRequestTask: Map[String, MixRequestTask]) extends Account {

  val address: String = reservedBalanceAccount.address
  val startingBalance: BigDecimal = reservedBalanceAccount.startingBalance
  val currentBalance: BigDecimal = reservedBalanceAccount.currentBalance
  val ledger: Map[String, Transaction] = reservedBalanceAccount.ledger
  val reservedBalance: BigDecimal = reservedBalanceAccount.reservedBalance

  def balanceAvailableForWithdrawal(balance: BigDecimal): Boolean = balance >= currentBalance

  def fundAvailableForMixRequestTasks(mixRequestTasks: Seq[MixRequestTask]): Boolean = balanceAvailableForWithdrawal(mixRequestTasks.map(_.transaction.amount).sum)

  def addScheduledMixRequestTasks(mixRequestTasks: Seq[MixRequestTask]): MixingAccount = ???

  def addCompletedMixRequestTasks(mixRequestTasks: Seq[MixRequestTask]): MixingAccount = ???
}