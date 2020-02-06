package com.gemini.jobcoin.mixrequest

import java.time.LocalDateTime
import java.util.UUID

trait MixRequest extends MixRequestCoordinateDelegate {
  val id: String
  val initiatedAt: LocalDateTime
  val mixingAddress: String
  val mixRequestCoordinate: MixRequestCoordinate
}

case class MixRequestImpl(id: String, mixingAddress: String, initiatedAt: LocalDateTime, mixRequestCoordinate: MixRequestCoordinate)
  extends MixRequest
    with MixRequestCoordinate

object MixRequest {
  def apply(id: String, mixingAddress: String, initiatedAt: LocalDateTime, mixRequestCoordinate: MixRequestCoordinate): MixRequest =
    MixRequestImpl(id, mixingAddress, initiatedAt, mixRequestCoordinate)

  def apply(mixingAddress: String, initiatedAt: LocalDateTime, mixRequestCoordinate: MixRequestCoordinate): MixRequest =
    MixRequest(UUID.randomUUID().toString, mixingAddress, initiatedAt, mixRequestCoordinate)
}

trait MixRequestDelegate extends MixRequest {
  val mixRequest: MixRequest
  val id: String = mixRequest.id
  val mixingAddress: String = mixRequest.mixingAddress
  val mixRequestCoordinate: MixRequestCoordinate = mixRequest.mixRequestCoordinate
  val initiatedAt: LocalDateTime = mixRequest.initiatedAt
}



