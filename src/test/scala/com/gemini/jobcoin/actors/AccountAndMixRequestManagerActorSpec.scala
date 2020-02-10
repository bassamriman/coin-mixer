package com.gemini.jobcoin.actors

import java.time.LocalDateTime

import akka.actor.{ActorRef, ActorSystem}
import akka.testkit.{ImplicitSender, TestKit, TestProbe}
import com.gemini.jobcoin.accounting.{IdentifiableTransaction, Transaction}
import com.gemini.jobcoin.mixrequest.models.{
  MixRequest,
  MixRequestCoordinate,
  MixRequestWithBalance,
  MixingProperties
}

import org.scalatest.{BeforeAndAfterAll, FlatSpecLike, Matchers}

class AccountAndMixRequestManagerActorSpec
    extends TestKit(ActorSystem("AccountManagerActorSpec"))
    with FlatSpecLike
    with ImplicitSender
    with Matchers
    with BeforeAndAfterAll {

  val seed: Long = 1038199

  "AccountManagerActor" should "complete a full cycle" in {

    val testProb: TestProbe = TestProbe()
    val newRequestDispatcherActor: TestProbe = TestProbe()
    val balanceMonitorActor: TestProbe = TestProbe()
    val mixedTransactionGeneratorActor: TestProbe = TestProbe()
    val committerActor: TestProbe = TestProbe()
    val validatorActor: TestProbe = TestProbe()

    val address: String = "Mixing Address"
    val mixingProperties: MixingProperties = MixingProperties(
      minTransactionPerDestinationAddress = 10,
      maxTransactionPerDestinationAddress = 20,
      minTransactionAmount = BigDecimal(0),
      maxTransactionAmount = BigDecimal(10),
      maxScale = 3
    )

    val accountManagerActor: ActorRef = system.actorOf(
      AccountAndMixRequestManagerActor.props(
        address = address,
        mixingProperties = mixingProperties,
        numberOfMixRequestTaskToSchedule = 10,
        initialSeed = seed,
        balanceMonitorActor = balanceMonitorActor.ref,
        mixedTransactionGeneratorActor = mixedTransactionGeneratorActor.ref,
        committerActor = committerActor.ref,
        validatorActor = validatorActor.ref
      )
    )

    // Initiate a MixRequest
    val requestAt: LocalDateTime = LocalDateTime.now()
    val sourceAddress = "Source Address 1"
    val destinationAddress = "Destination Address 1"
    val mixRequestCoordinates =
      Seq(MixRequestCoordinate(sourceAddress, Seq(destinationAddress)))
    newRequestDispatcherActor.send(
      accountManagerActor,
      NewRequestDispatcherActor
        .NewRequests(mixRequestCoordinates, requestAt)
    )

    // BalanceReceived for MixRequest
    val balanceMonitorActorMsg: BalanceMonitorActor.NewRequestsAwaitingBalance =
      balanceMonitorActor
        .expectMsgType[BalanceMonitorActor.NewRequestsAwaitingBalance]
    val mixRequests = balanceMonitorActorMsg.mixRequest

    val balance = BigDecimal(43.4)
    val balanceMixRequestPairs: Seq[(BigDecimal, MixRequest)] =
      Seq(balance -> mixRequests.head)
    val balanceReceivedAt = LocalDateTime.now()
    balanceMonitorActor.send(
      accountManagerActor,
      BalanceMonitorActor
        .BalanceReceived(balanceMixRequestPairs, balanceReceivedAt)
    )

    //Schedule Source to Mixing Transaction
    accountManagerActor ! AccountAndMixRequestManagerActor.ScheduleNewMixRequestTasks

    //Commit Transaction
    val committerActorMsg: CommitterActor.Commit =
      committerActor
        .expectMsgType[CommitterActor.Commit]
    val committedAt: LocalDateTime = LocalDateTime.now()
    committerActor.send(
      accountManagerActor,
      CommitterActor
        .Committed(Seq(committerActorMsg.mixRequestTask), committedAt)
    )

    //Validate Transaction
    val validatorActorMsg: ValidatorActor.Validate =
      validatorActor
        .expectMsgType[ValidatorActor.Validate]
    val validatedAt: LocalDateTime = LocalDateTime.now()
    validatorActor.send(
      accountManagerActor,
      ValidatorActor.Validated(validatorActorMsg.mixRequestTasks, validatedAt)
    )

    // Mixed Transactions for MixRequest for received
    val mixedTransactionGeneratorActorMsg
      : MixedTransactionGeneratorActor.GenerateMixedTransactions =
      mixedTransactionGeneratorActor
        .expectMsgType[MixedTransactionGeneratorActor.GenerateMixedTransactions]
    val mixRequestWithBalance: MixRequestWithBalance =
      mixedTransactionGeneratorActorMsg.mixRequests.head
    val transactions = Seq(Transaction.withId(sourceAddress, address, balance))
    val mixRequestTransactionsPairs
      : Seq[(MixRequestWithBalance, Seq[IdentifiableTransaction])] =
      Seq(mixRequestWithBalance -> transactions)
    val mixedRequestReceived: LocalDateTime = LocalDateTime.now()
    mixedTransactionGeneratorActor.send(
      accountManagerActor,
      MixedTransactionGeneratorActor
        .MixedTransactions(mixRequestTransactionsPairs, mixedRequestReceived)
    )

    //Schedule mixed transaction
    accountManagerActor ! AccountAndMixRequestManagerActor.ScheduleNewMixRequestTasks

    //Commit Transaction
    val committerActorMsg2: CommitterActor.Commit =
      committerActor
        .expectMsgType[CommitterActor.Commit]
    val committedAt2: LocalDateTime = LocalDateTime.now()
    committerActor.send(
      accountManagerActor,
      CommitterActor
        .Committed(Seq(committerActorMsg2.mixRequestTask), committedAt2)
    )

    //Validate Transaction
    val validatorActorMsg2: ValidatorActor.Validate =
      validatorActor
        .expectMsgType[ValidatorActor.Validate]
    val validatedAt2: LocalDateTime = LocalDateTime.now()
    validatorActor.send(
      accountManagerActor,
      ValidatorActor.Validated(validatorActorMsg2.mixRequestTasks, validatedAt2)
    )

    testProb.send(
      accountManagerActor,
      AccountAndMixRequestManagerActor.GetStatus
    )
    //val result = testProb.expectMsgType[AccountManagerActor.Status]

    //val a = 1

  }
}
