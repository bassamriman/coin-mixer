package com.gemini.jobcoin

import com.gemini.jobcoin.common.RandomNumberGenerator
import org.scalatest._

class RandomNumberGeneratorSpec extends FlatSpec with Matchers {
  val seed: Long = 210

  "generateRandomBigDecimalBetween method" should "generate number 74.0930 given min=1 and max=100" in {
    val min = 1
    val max = 100
    val maxScale = 4
    val output =
      RandomNumberGenerator.generateRandomBigDecimalBetween(min, max, maxScale)(
        seed
      )
    val expectedOutput = BigDecimal(87.86)
    output should be(expectedOutput)
  }

  "generateRandomBigDecimalBetween method" should "generate number 0 given min=0 and max=0" in {
    val min = 0
    val max = 0
    val maxScale = 1
    val output = RandomNumberGenerator
      .generateRandomBigDecimalBetween(min, max, maxScale)(seed)
      .toString
    val expectedOutput = BigDecimal(0).setScale(0).toString
    output should be(expectedOutput)
  }

  "generateRandomBigDecimals method" should "generate bigdecimal numbers that sum to 232910.913199282" in {
    val min = 0
    val max = 10000
    val maxScale = 6
    val sum = BigDecimal(232910.913199282)
    val output: Seq[BigDecimal] =
      RandomNumberGenerator.generateRandomBigDecimals(sum, min, max, maxScale)(
        seed
      )
    val expectedOutput = sum
    output.sum should be(expectedOutput)
  }

  "generateRandomInts method" should "generate integer numbers that sum to 232910" in {
    val min = 0
    val max = 10000
    val sum = 232910
    val output: Seq[Int] =
      RandomNumberGenerator.generateRandomInts(sum, min, max)(seed)
    val expectedOutput = sum
    output.sum should be(expectedOutput)
  }

  "generateRandomInts method" should "generate 10 integer numbers that sum to 100" in {
    val n = 10
    val sum = 100
    val output: Seq[Int] =
      RandomNumberGenerator.generateRandomInts(sum, n)(seed)
    val expectedOutput = sum
    output.sum should be(expectedOutput)
  }

  "generateRandomInts method" should "generate 0 integer numbers that sum to 100" in {
    val n = 0
    val sum = 100
    val output: Seq[Int] =
      RandomNumberGenerator.generateRandomInts(sum, n)(seed)
    val expectedOutput = 0
    output.sum should be(expectedOutput)
  }
}
