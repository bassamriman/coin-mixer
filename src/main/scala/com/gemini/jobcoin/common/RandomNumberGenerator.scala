package com.gemini.jobcoin.common

import scala.annotation.tailrec
import scala.util.Random

/**
  * API that allows us to generates ranges of numbers
  */
object RandomNumberGenerator {

  /**
    * Generate an Int between a minimum and maximum
    * @param min
    * @param max
    * @param seed
    * @return Int
    */
  def generateRandomIntBetween(min: Int, max: Int)(seed: Long): Int = {
    require(max > min, "max should be bigger then min")
    require(min >= 0, "min should be bigger than zero")
    require(max >= 0, "max should be bigger than zero")
    val newSeed = new Random(seed).nextLong()
    min + new Random(newSeed).nextInt(max - min)
  }

  /**
    * Generate a Big decimal between a minimum and maximum
    * @param min
    * @param max
    * @param maxScale
    * @param seed
    * @return bigdecimal
    */
  def generateRandomBigDecimalBetween(min: BigDecimal,
                                      max: BigDecimal,
                                      maxScale: Int)(seed: Long): BigDecimal = {
    require(max >= min, "max should be bigger then min")
    require(min >= 0, "min should be bigger than zero")
    require(max >= 0, "max should be bigger than zero")
    require(maxScale > 0, "maxScale should be bigger than zero")
    val newSeed = new Random(seed).nextLong()
    val scale = generateRandomIntBetween(0, maxScale)(newSeed)
    val generatedBigDecimal = min + (BigDecimal(
      new Random(newSeed).nextDouble()
    ) * (max - min))
    generatedBigDecimal.setScale(scale, BigDecimal.RoundingMode.HALF_UP)
  }

  /**
    * Generate a sequence of bigdecimal between a minimum and a maximum
    * that sum up to certain amount
    * @param sum
    * @param min
    * @param max
    * @param maxScale
    * @param seed
    * @return bigdecimal
    */
  def generateRandomBigDecimals(sum: BigDecimal,
                                min: BigDecimal,
                                max: BigDecimal,
                                maxScale: Int)(seed: Long): Seq[BigDecimal] = {
    require(max >= min, "max should be bigger then min")
    require(min >= 0, "min should be bigger than zero")
    require(max >= 0, "max should be bigger than zero")
    require(maxScale > 0, "maxScale should be bigger than zero")
    require(sum >= 0, "max should be bigger than zero")
    val newSeed = new Random(seed).nextLong()
    combineUntilDesiredAccumulatedAmount(
      accumulatedSum = BigDecimal(0),
      generatedNumbers = Seq.empty,
      desiredAmount = sum,
      sumCombinator = (a: BigDecimal, b: BigDecimal) => a + b,
      minusCombinator = (a: BigDecimal, b: BigDecimal) => a - b,
      equalsPredicate = (a: BigDecimal, b: BigDecimal) => a == b,
      superiorPredicate = (a: BigDecimal, b: BigDecimal) => a > b,
      generateNumber = generateRandomBigDecimalBetween(min, max, maxScale)
    )(newSeed)
  }

  /**
    * Generate a certain number of ints that sum to a certain amount
    * @param sum
    * @param numberOfInts
    * @param seed
    * @return
    */
  def generateRandomInts(sum: Int, numberOfInts: Int)(seed: Long): Seq[Int] = {
    require(sum > 0, "sum should be bigger than zero")
    val newSeed = new Random(seed).nextLong()
    val ints: Seq[Int] =
      (1 to numberOfInts).map(
        i => new Random(new Random(newSeed + i).nextLong()).nextInt(2 * sum)
      )
    val sumOfInt: Int = ints.sum
    val normalizingRatio: Double = sum.toDouble / sumOfInt.toDouble
    val normalizedInts: Seq[Int] = ints.map(_ * normalizingRatio).map(_.toInt)
    val sumOfNormalizedInt: Int = normalizedInts.sum
    val adjustment: Int = sum - sumOfNormalizedInt
    //TODO: Add adjustment to a random int in the int seq
    val adjustFirstInt = normalizedInts.headOption.map(_ + adjustment)
    adjustFirstInt.map(_ +: normalizedInts.tail).getOrElse(Seq.empty)
  }

  /**
    * Generate a certain number of ints between a minimum and maximum that sum to a certain amount
    * @param sum
    * @param seed
    * @return
    */
  def generateRandomInts(sum: Int, min: Int, max: Int)(seed: Long): Seq[Int] = {
    require(max >= min, "max should be bigger then min")
    require(min >= 0, "min should be bigger than zero")
    require(max >= 0, "max should be bigger than zero")
    require(sum >= 0, "sum should be bigger than zero")
    val newSeed = new Random(seed).nextLong()
    combineUntilDesiredAccumulatedAmount(
      accumulatedSum = 0,
      generatedNumbers = Seq.empty,
      desiredAmount = sum,
      sumCombinator = (a: Int, b: Int) => a + b,
      minusCombinator = (a: Int, b: Int) => a - b,
      equalsPredicate = (a: Int, b: Int) => a == b,
      superiorPredicate = (a: Int, b: Int) => a > b,
      generateNumber = generateRandomIntBetween(min, max)
    )(newSeed)
  }

  @tailrec
  private def combineUntilDesiredAccumulatedAmount[T](
    accumulatedSum: T,
    generatedNumbers: Seq[T],
    desiredAmount: T,
    sumCombinator: (T, T) => T,
    minusCombinator: (T, T) => T,
    equalsPredicate: (T, T) => Boolean,
    superiorPredicate: (T, T) => Boolean,
    generateNumber: Long => T
  )(seed: Long): Seq[T] = {
    if (equalsPredicate(accumulatedSum, desiredAmount)) {
      generatedNumbers
    } else {
      val newNumber: T = generateNumber(seed + 1)
      val newAccumulatedSum = sumCombinator(newNumber, accumulatedSum)
      val (adjustedNewNumber, adjustedNewAccumulatedSum) =
        if (superiorPredicate(newAccumulatedSum, desiredAmount)) {
          val adjustment = minusCombinator(newAccumulatedSum, desiredAmount)
          (
            minusCombinator(newNumber, adjustment),
            minusCombinator(newAccumulatedSum, adjustment)
          )
        } else (newNumber, newAccumulatedSum)
      combineUntilDesiredAccumulatedAmount(
        adjustedNewAccumulatedSum,
        generatedNumbers :+ adjustedNewNumber,
        desiredAmount,
        sumCombinator,
        minusCombinator,
        equalsPredicate,
        superiorPredicate,
        generateNumber
      )(seed + 1)
    }
  }
}
