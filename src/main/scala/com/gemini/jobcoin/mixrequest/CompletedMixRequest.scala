package com.gemini.jobcoin.mixrequest

import java.time.LocalDateTime

trait CompletedMixRequest extends MixingMixRequestDelegate {
  val mixingMixRequest: MixingMixRequest
}

case class CompletedMixRequestImpl(completedAt: LocalDateTime, mixingMixRequest: MixingMixRequest)
  extends CompletedMixRequest
    with MixingMixRequest
    with MixRequestWithBalance
    with MixRequest
    with MixRequestCoordinate

object CompletedMixRequest {
  def apply(completedAt: LocalDateTime, mixingMixRequest: MixingMixRequest): CompletedMixRequest = CompletedMixRequestImpl(completedAt, mixingMixRequest)
}