package com.gemini.jobcoin.mixrequest

import com.gemini.jobcoin.accounting.Transaction

trait MixRequestWithBalance extends MixRequestDelegate {
  val sourceAddressBalance: BigDecimal
  val sourceAddressToMixingAddressMixRequestTask: MixRequestTask
  val mixRequest: MixRequest
}

case class MixRequestWithBalanceImpl(sourceAddressBalance: BigDecimal,
                                     sourceAddressToMixingAddressMixRequestTask: MixRequestTask,
                                     mixRequest: MixRequest)
  extends MixRequestWithBalance
    with MixRequest
    with MixRequestCoordinate

object MixRequestWithBalance {
  def apply(sourceAddressBalance: BigDecimal,
            sourceAddressToMixingAddressMixRequestTask: MixRequestTask,
            mixRequest: MixRequest): MixRequestWithBalance =
    MixRequestWithBalanceImpl(sourceAddressBalance, sourceAddressToMixingAddressMixRequestTask, mixRequest)

  def apply(sourceAddressBalance: BigDecimal, mixRequest: MixRequest): MixRequestWithBalance =
    MixRequestWithBalance(
      sourceAddressBalance,
      MixRequestTask(mixRequest.id,
        Transaction.withId(
          mixRequest.sourceAddress,
          mixRequest.mixingAddress,
          sourceAddressBalance)),
      mixRequest)
}

trait MixRequestWithBalanceDelegate extends MixRequestWithBalance {
  val mixRequestWithBalance: MixRequestWithBalance
  val sourceAddressBalance: BigDecimal = mixRequestWithBalance.sourceAddressBalance
  val sourceAddressToMixingAddressMixRequestTask: MixRequestTask =
    mixRequestWithBalance.sourceAddressToMixingAddressMixRequestTask
  val mixRequest: MixRequest = mixRequestWithBalance.mixRequest
}