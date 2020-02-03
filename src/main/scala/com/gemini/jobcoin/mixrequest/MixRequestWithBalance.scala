package com.gemini.jobcoin.mixrequest

trait MixRequestWithBalance extends MixRequestDelegate {
  val sourceAddressBalance: BigDecimal
  val mixRequest: MixRequest
}

case class MixRequestWithBalanceImpl(sourceAddressBalance: BigDecimal, mixRequest: MixRequest)
  extends MixRequestWithBalance
    with MixRequest
    with MixRequestCoordinate

object MixRequestWithBalance {
  def apply(sourceAddressBalance: BigDecimal, mixRequest: MixRequest): MixRequestWithBalance =
    MixRequestWithBalanceImpl(sourceAddressBalance, mixRequest)
}

trait MixRequestWithBalanceDelegate extends MixRequestWithBalance {
  val mixRequestWithBalance: MixRequestWithBalance
  val sourceAddressBalance: BigDecimal = mixRequestWithBalance.sourceAddressBalance
  val mixRequest: MixRequest = mixRequestWithBalance.mixRequest
}