package com.gemini.jobcoin

import scala.math.BigDecimal.RoundingMode
import scala.util.Random

object RandomNumberGenerator {
  def generateRandomIntBetween(min: Int, max: Int)(seed: Long): Int = {
    require(max > min, "max should be bigger then min")
    min + new Random(seed).nextInt(max - min + 1)
  }

  def generateRandomBigDecimalBetween(min: BigDecimal, max: BigDecimal, maxScale: Int)(seed: Long): BigDecimal = {
    require(max > min, "max should be bigger then min")
    val scale = generateRandomIntBetween(0, maxScale)(seed)
    val generatedBigdecimal = min + (BigDecimal(new Random(seed).nextDouble()) * (max - min))
    generatedBigdecimal.setScale(scale, BigDecimal.RoundingMode.HALF_UP)
  }

  def generateRandomBigDecimals(sum: BigDecimal, min: BigDecimal, max: BigDecimal, maxScale: Int)(seed: Long): Seq[BigDecimal] = {
    val numberOfNumbersToGenerate: Int = (sum / min).setScale(0, RoundingMode.UP).toInt
    val generatedNumbers: Seq[BigDecimal] = 1 to numberOfNumbersToGenerate map (i => generateRandomBigDecimalBetween(min, max, maxScale)(seed + i))
    val sumOfGeneratedNumbers: BigDecimal = generatedNumbers.sum
    generatedNumbers.map(number => number / sumOfGeneratedNumbers * sum)
  }

  def generateRandomInts(sum: Int, min: Int, max: Int)(seed: Long): Seq[Int] = {
    val numberOfNumbersToGenerate: Int = sum / min
    val generatedNumbers: Seq[Int] = 1 to numberOfNumbersToGenerate map (i => generateRandomIntBetween(min, max)(seed + i))
    val sumOfGeneratedNumbers: Int = generatedNumbers.sum
    val generatedNumberWithAdjustedSum: Seq[Int] = generatedNumbers.map(number => number / sumOfGeneratedNumbers * sum)
    val sumOfGeneratedNumberWithAdjustedSum: Int = generatedNumberWithAdjustedSum.sum
    val adjustment = sum - sumOfGeneratedNumberWithAdjustedSum
    val adjustedTailOfGeneratedNumberWithAdjustedSum: Int = generatedNumberWithAdjustedSum.last + adjustment
    generatedNumberWithAdjustedSum.dropRight(1) :+ adjustedTailOfGeneratedNumberWithAdjustedSum
  }
}
