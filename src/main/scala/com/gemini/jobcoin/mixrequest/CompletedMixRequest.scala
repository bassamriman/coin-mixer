package com.gemini.jobcoin.mixrequest

import java.time.LocalDateTime

trait CompletedMixRequest extends MixRequestWithBalanceDelegate {
  val mixRequestWithBalance: MixRequestWithBalance
}

case class CompletedMixRequestImpl(completedAt: LocalDateTime, mixRequestWithBalance: MixRequestWithBalance)
  extends CompletedMixRequest
    with MixRequestWithBalance
    with MixRequest
    with MixRequestCoordinate

object CompletedMixRequest {
  def apply(completedAt: LocalDateTime, mixRequestWithBalance: MixRequestWithBalance): CompletedMixRequest =
    CompletedMixRequestImpl(completedAt, mixRequestWithBalance)
}