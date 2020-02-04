package com.gemini.jobcoin.mixrequest

import java.time.LocalDateTime
import java.util.UUID

trait MixRequest extends MixRequestCoordinateDelegate {
  val id: String
  val initiatedAt: LocalDateTime
  val mixRequestCoordinate: MixRequestCoordinate
}

case class MixRequestImpl(id: String, initiatedAt: LocalDateTime, mixRequestCoordinate: MixRequestCoordinate)
  extends MixRequest
    with MixRequestCoordinate

object MixRequest {
  def apply(id: String, initiatedAt: LocalDateTime, mixRequestCoordinate: MixRequestCoordinate): MixRequest =
    MixRequestImpl(id, initiatedAt, mixRequestCoordinate)

  def apply(initiatedAt: LocalDateTime, mixRequestCoordinate: MixRequestCoordinate): MixRequest =
    MixRequest(UUID.randomUUID().toString, initiatedAt, mixRequestCoordinate)
}

trait MixRequestDelegate extends MixRequest {
  val mixRequest: MixRequest
  val id: String = mixRequest.id
  val mixRequestCoordinate: MixRequestCoordinate = mixRequest.mixRequestCoordinate
  val initiatedAt: LocalDateTime = mixRequest.initiatedAt
}



