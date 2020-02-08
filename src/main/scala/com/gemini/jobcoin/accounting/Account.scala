package com.gemini.jobcoin.accounting

import com.gemini.jobcoin.mixrequest.MixRequestTask

trait Account {
  val address: String
  val startingBalance: BigDecimal
  val currentBalance: BigDecimal
  val ledger: IdentifiableTransactionLedger
  def +(transaction: IdentifiableTransaction): StandardAccount
  def ++(transactions: Seq[IdentifiableTransaction]): StandardAccount
}

case class StandardAccount(address: String,
                           startingBalance: BigDecimal,
                           ledger: IdentifiableTransactionLedger)
    extends Account {
  val currentBalance: BigDecimal = startingBalance + ledger.balance(address)

  def +(transaction: IdentifiableTransaction): StandardAccount =
    this.copy(ledger = ledger + transaction)
  def ++(transactions: Seq[IdentifiableTransaction]): StandardAccount = {
    this.copy(ledger = ledger ++ transactions)
  }

  def -(transaction: IdentifiableTransaction): StandardAccount =
    this.copy(ledger = ledger - transaction)

  def --(transactions: Seq[IdentifiableTransaction]): StandardAccount =
    this.copy(
      ledger = ledger -- transactions.filter(_.involvesAddress(address))
    )
}

object StandardAccount {
  def empty(address: String): StandardAccount =
    StandardAccount(address, 0, Ledger.emptyIdentifiableTransactionLedger)
}

case class ReservableBalanceAccount(address: String,
                                    account: StandardAccount,
                                    reservedBalanceAccount: StandardAccount) {
  val startingBalance
    : BigDecimal = account.startingBalance + reservedBalanceAccount.startingBalance
  val currentBalance
    : BigDecimal = account.currentBalance + reservedBalanceAccount.currentBalance
  val reservedBalance: BigDecimal = -reservedBalanceAccount.currentBalance

  val ledger
    : IdentifiableTransactionLedger = account.ledger + reservedBalanceAccount.ledger

  def allocatedAmount(
    transactions: Seq[IdentifiableTransaction]
  ): (ReservableBalanceAccount, Seq[IdentifiableTransaction]) = {
    val sendingTransactions: Seq[IdentifiableTransaction] =
      transactions
        .flatMap(
          t =>
            t.basicTransaction
              .forcePositiveBalance(address)
              .map(
                adjustedTransaction =>
                  t.copy(basicTransaction = adjustedTransaction)
            )
        )
        .filter(_.isSendingAddress(address))

    this.copy(
      reservedBalanceAccount = reservedBalanceAccount ++ sendingTransactions
    ) ->
      sendingTransactions

  }

  def commitTransactions(
    transactions: Seq[IdentifiableTransaction]
  ): (ReservableBalanceAccount, Seq[IdentifiableTransaction]) = {
    val newReservedBalanceAccount
      : StandardAccount = reservedBalanceAccount -- transactions
    val newAccount: StandardAccount = account ++ transactions
    this.copy(
      account = newAccount,
      reservedBalanceAccount = newReservedBalanceAccount
    ) -> transactions
  }

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
                         completedMixRequestTask: Map[String, MixRequestTask]) {

  val address: String = reservedBalanceAccount.address
  val startingBalance: BigDecimal = reservedBalanceAccount.startingBalance
  val currentBalance: BigDecimal = reservedBalanceAccount.currentBalance
  val ledger: IdentifiableTransactionLedger = reservedBalanceAccount.ledger
  val reservedBalance: BigDecimal = reservedBalanceAccount.reservedBalance

  def balanceAvailableForWithdrawal(balance: BigDecimal): Boolean =
    balance <= currentBalance

  def fundAvailableForMixRequestTasks(
    mixRequestTasks: Seq[MixRequestTask]
  ): Boolean = {
    val sendingMixRequestTask =
      mixRequestTasks.filter(mrt => mrt.transaction.isSendingAddress(address))
    balanceAvailableForWithdrawal(
      sendingMixRequestTask.map(_.transaction.amount).sum
    )
  }

  def addScheduledMixRequestTasks(
    mixRequestTasks: Seq[MixRequestTask]
  ): MixingAccount = {
    val (newReservedBalanceAccount, allocatedTransactions) =
      reservedBalanceAccount.allocatedAmount(mixRequestTasks.map(_.transaction))
    val indexAllocatedTransactions =
      allocatedTransactions.map(t => t.id -> t).toMap
    val newScheduledMixRequestTask
      : Map[String, MixRequestTask] = scheduledMixRequestTask ++
      mixRequestTasks
        .map(mixRequestTask => mixRequestTask.id -> mixRequestTask)
        .filter(
          entry => indexAllocatedTransactions.contains(entry._2.transaction.id)
        )
    this.copy(
      reservedBalanceAccount = newReservedBalanceAccount,
      scheduledMixRequestTask = newScheduledMixRequestTask
    )
  }

  def addCompletedMixRequestTasks(
    mixRequestTasks: Seq[MixRequestTask]
  ): MixingAccount = {
    val (newReservedBalanceAccount, completedTransaction) =
      reservedBalanceAccount.commitTransactions(
        mixRequestTasks.map(_.transaction)
      )
    val indexedCompletedTransactions: Map[String, IdentifiableTransaction] =
      completedTransaction.map(t => t.id -> t).toMap

    val newCompletedMixRequestTask
      : Map[String, MixRequestTask] = scheduledMixRequestTask ++
      mixRequestTasks
        .map(mixRequestTask => mixRequestTask.id -> mixRequestTask)
        .filter(
          entry =>
            indexedCompletedTransactions.contains(entry._2.transaction.id)
        )

    val newScheduledMixRequestTask = scheduledMixRequestTask -- newCompletedMixRequestTask.keys
    this.copy(
      reservedBalanceAccount = newReservedBalanceAccount,
      scheduledMixRequestTask = newScheduledMixRequestTask,
      completedMixRequestTask = newCompletedMixRequestTask
    )
  }
}

object MixingAccount {
  def empty(address: String): MixingAccount =
    MixingAccount(ReservableBalanceAccount.empty(address), Map.empty, Map.empty)
}
