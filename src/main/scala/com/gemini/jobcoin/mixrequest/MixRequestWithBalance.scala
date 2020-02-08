package com.gemini.jobcoin.mixrequest

import com.gemini.jobcoin.accounting.Transaction

trait MixRequestWithBalance extends MixRequestDelegate {
  def sourceAddressBalance: BigDecimal
  def sourceAddressToMixingAddressMixRequestTask: MixRequestTask
  def mixRequest: MixRequest
}

case class MixRequestWithBalanceImpl(
  sourceAddressBalance: BigDecimal,
  sourceAddressToMixingAddressMixRequestTask: MixRequestTask,
  mixRequest: MixRequest
) extends MixRequestWithBalance
    with MixRequest
    with MixRequestCoordinate

object MixRequestWithBalance {
  def apply(sourceAddressBalance: BigDecimal,
            sourceAddressToMixingAddressMixRequestTask: MixRequestTask,
            mixRequest: MixRequest): MixRequestWithBalance =
    MixRequestWithBalanceImpl(
      sourceAddressBalance,
      sourceAddressToMixingAddressMixRequestTask,
      mixRequest
    )

  def apply(sourceAddressBalance: BigDecimal,
            mixRequest: MixRequest): MixRequestWithBalance =
    MixRequestWithBalance(
      sourceAddressBalance,
      MixRequestTask(
        mixRequest.id,
        Transaction.withId(
          mixRequest.sourceAddress,
          mixRequest.mixingAddress,
          sourceAddressBalance
        )
      ),
      mixRequest
    )
}

trait MixRequestWithBalanceDelegate extends MixRequestWithBalance {
  def mixRequestWithBalance: MixRequestWithBalance
  def sourceAddressBalance: BigDecimal =
    mixRequestWithBalance.sourceAddressBalance
  def sourceAddressToMixingAddressMixRequestTask: MixRequestTask =
    mixRequestWithBalance.sourceAddressToMixingAddressMixRequestTask
  def mixRequest: MixRequest = mixRequestWithBalance.mixRequest
}
