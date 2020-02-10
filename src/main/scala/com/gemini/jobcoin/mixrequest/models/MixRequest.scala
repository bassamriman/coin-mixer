package com.gemini.jobcoin.mixrequest.models

import java.time.LocalDateTime
import java.util.UUID

/**
  * Adds an id, time of creation and mixing address to MixRequestCoordinate
  */
trait MixRequest extends MixRequestCoordinateDelegate {
  def id: String
  def initiatedAt: LocalDateTime
  def mixingAddress: String
  def mixRequestCoordinate: MixRequestCoordinate
}

case class MixRequestImpl(id: String,
                          mixingAddress: String,
                          initiatedAt: LocalDateTime,
                          mixRequestCoordinate: MixRequestCoordinate)
    extends MixRequest
    with MixRequestCoordinate

object MixRequest {
  def apply(id: String,
            mixingAddress: String,
            initiatedAt: LocalDateTime,
            mixRequestCoordinate: MixRequestCoordinate): MixRequest =
    MixRequestImpl(id, mixingAddress, initiatedAt, mixRequestCoordinate)

  def apply(mixingAddress: String,
            initiatedAt: LocalDateTime,
            mixRequestCoordinate: MixRequestCoordinate): MixRequest =
    MixRequest(
      UUID.randomUUID().toString,
      mixingAddress,
      initiatedAt,
      mixRequestCoordinate
    )
}

trait MixRequestDelegate extends MixRequest {
  def mixRequest: MixRequest
  def id: String = mixRequest.id
  def mixingAddress: String = mixRequest.mixingAddress
  def mixRequestCoordinate: MixRequestCoordinate =
    mixRequest.mixRequestCoordinate
  def initiatedAt: LocalDateTime = mixRequest.initiatedAt
}
