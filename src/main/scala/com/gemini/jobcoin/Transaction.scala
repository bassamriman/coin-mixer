package com.gemini.jobcoin

import java.util.UUID

case class Transaction(fromAddress: String,
                       toAddress: String,
                       amount: BigDecimal, id: String = UUID.randomUUID.toString) extends Identifiable {
  def signAdjustAmount(address: String): BigDecimal =
    if (fromAddress == address && toAddress == address) {
      BigDecimal(0)
    } else if (fromAddress == address) {
      -amount
    } else if (toAddress == address) {
      amount
    } else {
      throw new IllegalArgumentException("Given address is not part of the transaction")
    }
}
