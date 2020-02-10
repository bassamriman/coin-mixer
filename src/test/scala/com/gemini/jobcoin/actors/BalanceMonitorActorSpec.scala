package com.gemini.jobcoin.actors

import java.time.LocalDateTime

import akka.actor.{ActorRef, ActorSystem}
import akka.testkit.{ImplicitSender, TestKit, TestProbe}
import com.gemini.jobcoin.accounting.{BasicLedger, BasicTransaction}
import com.gemini.jobcoin.mixrequest.models.{MixRequest, MixRequestCoordinate}
import org.scalatest.{BeforeAndAfterAll, FlatSpecLike, Matchers}

class BalanceMonitorActorSpec
    extends TestKit(ActorSystem("BalanceMonitorActorSpec"))
    with FlatSpecLike
    with ImplicitSender
    with Matchers
    with BeforeAndAfterAll {
  val mixingAddress: String = "Mixing Address"
  val destinationAddress1: String = "Destination Address 1"
  val destinationAddress2: String = "Destination Address 2"
  def mixRequest(id: String, sourceAddress: String): MixRequest = {
    val mixRequestId: String = id

    val initiatedAt: LocalDateTime = LocalDateTime.now()
    val mixRequestCoordinate: MixRequestCoordinate = MixRequestCoordinate(
      sourceAddress,
      Seq(destinationAddress1, destinationAddress2)
    )
    MixRequest(
      id = mixRequestId,
      mixingAddress = mixingAddress,
      initiatedAt = initiatedAt,
      mixRequestCoordinate = mixRequestCoordinate
    )
  }
  "BalanceMonitorActor" should "return correct Message" in {

    val testProb: TestProbe = TestProbe()
    val balanceMonitorActor: ActorRef =
      system.actorOf(BalanceMonitorActor.props)

    val sourceAddress1: String = "Source Address 1"
    val sourceAddress2: String = "Source Address 2"
    val mixRequest1 = mixRequest("Mix Request 1", sourceAddress1)
    val mixRequest2 = mixRequest("Mix Request 2", sourceAddress2)

    testProb.send(
      balanceMonitorActor,
      BalanceMonitorActor.NewRequestsAwaitingBalance(
        Seq(mixRequest1, mixRequest2)
      )
    )

    val amount: BigDecimal = BigDecimal(32.1)
    val transaction1 =
      BasicTransaction("external 1", sourceAddress1, BigDecimal(32.1))
    val ledger = BasicLedger.apply(Seq(transaction1))
    testProb.send(
      balanceMonitorActor,
      LedgerPublisherActor.LatestLedger(ledger)
    )

    val output: Seq[(BigDecimal, MixRequest)] =
      testProb
        .expectMsgType[BalanceMonitorActor.BalanceReceived]
        .balanceMixRequestPairs
    val expectedOutput = Seq(amount -> mixRequest1)
    output should be(expectedOutput)
  }
}
