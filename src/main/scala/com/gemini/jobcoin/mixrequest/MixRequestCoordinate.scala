package com.gemini.jobcoin.mixrequest

trait MixRequestCoordinate {
  val sourceAddress: String
  val destinationAddresses: Seq[String]
}

case class MixRequestCoordinateImpl(sourceAddress: String, destinationAddresses: Seq[String])
  extends MixRequestCoordinate

object MixRequestCoordinate {
  def apply(sourceAddress: String, destinationAddresses: Seq[String]): MixRequestCoordinate =
    MixRequestCoordinateImpl(sourceAddress, destinationAddresses)
}

trait MixRequestCoordinateDelegate {
  val mixRequestCoordinate: MixRequestCoordinate
  val sourceAddress: String = mixRequestCoordinate.sourceAddress
  val destinationAddresses: Seq[String] = mixRequestCoordinate.destinationAddresses
}