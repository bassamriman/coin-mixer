package com.gemini.jobcoin.accounting

import com.gemini.jobcoin.mixrequest.MixRequestTask

trait Account {
  val address: String
  val startingBalance: BigDecimal
  val currentBalance: BigDecimal
  val ledger: IdentifiableTransactionLedger

}

case class StandardAccount(address: String,
                           startingBalance: BigDecimal,
                           ledger: IdentifiableTransactionLedger)
    extends Account {
  val currentBalance: BigDecimal = startingBalance + ledger.balance(address)
}

object StandardAccount {
  def empty(address: String): StandardAccount =
    StandardAccount(address, 0, Ledger.emptyIdentifiableTransactionLedger)
}

case class ReservableBalanceAccount(address: String,
                                    account: Account,
                                    reservedBalanceAccount: Account)
    extends Account {
  val startingBalance
    : BigDecimal = account.startingBalance - reservedBalanceAccount.startingBalance
  val currentBalance
    : BigDecimal = account.currentBalance - reservedBalanceAccount.currentBalance
  val reservedBalance: BigDecimal = reservedBalanceAccount.currentBalance

  val ledger
    : IdentifiableTransactionLedger = account.ledger + reservedBalanceAccount.ledger

  def allocatedAmount(
    transactions: Seq[IdentifiableTransaction]
  ): ReservableBalanceAccount = ???

  def commitTransactions(
    transactions: Seq[IdentifiableTransaction]
  ): ReservableBalanceAccount = ???

}

object ReservableBalanceAccount {
  def empty(address: String): ReservableBalanceAccount =
    ReservableBalanceAccount(
      address,
      StandardAccount.empty(address),
      StandardAccount.empty(address)
    )
}

case class MixingAccount(reservedBalanceAccount: ReservableBalanceAccount,
                         scheduledMixRequestTask: Map[String, MixRequestTask],
                         completeMixRequestTask: Map[String, MixRequestTask])
    extends Account {

  val address: String = reservedBalanceAccount.address
  val startingBalance: BigDecimal = reservedBalanceAccount.startingBalance
  val currentBalance: BigDecimal = reservedBalanceAccount.currentBalance
  val ledger: IdentifiableTransactionLedger = reservedBalanceAccount.ledger
  val reservedBalance: BigDecimal = reservedBalanceAccount.reservedBalance

  def balanceAvailableForWithdrawal(balance: BigDecimal): Boolean =
    balance >= currentBalance

  def fundAvailableForMixRequestTasks(
    mixRequestTasks: Seq[MixRequestTask]
  ): Boolean =
    balanceAvailableForWithdrawal(mixRequestTasks.map(_.transaction.amount).sum)

  def addScheduledMixRequestTasks(
    mixRequestTasks: Seq[MixRequestTask]
  ): MixingAccount = ???

  def addCompletedMixRequestTasks(
    mixRequestTasks: Seq[MixRequestTask]
  ): MixingAccount = ???
}

object MixingAccount {
  def empty(address: String): MixingAccount =
    MixingAccount(ReservableBalanceAccount.empty(address), Map.empty, Map.empty)
}
