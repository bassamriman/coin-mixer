package com.gemini.jobcoin.mixrequest.models

import com.gemini.jobcoin.accounting.Transaction
import com.gemini.jobcoin.mixrequest.MixRequestTask

/**
  * Adds balance and the mix request task to a MixRequest.
  * The balance is money transferred to the address provided to the user.
  * The Mix Request Task tracks the state of the  transaction responsible
  * for transferring the funds from the given address to the mixing address
  */
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
