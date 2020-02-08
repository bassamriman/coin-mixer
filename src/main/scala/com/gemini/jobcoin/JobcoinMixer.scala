package com.gemini.jobcoin

import java.time.LocalDateTime
import java.util.UUID

import akka.actor.{ActorRef, ActorSystem}
import akka.stream.ActorMaterializer
import com.gemini.jobcoin.actors.{NewRequestDispatcherActor, SupervisorActor}
import com.gemini.jobcoin.mixrequest.{MixRequestCoordinate, MixingProperties}
import com.typesafe.config.ConfigFactory

import scala.concurrent.duration._
import scala.io.StdIn

object JobcoinMixer {

  object CompletedException extends Exception {}

  def main(args: Array[String]): Unit = {
    // Create an actor system
    implicit val actorSystem = ActorSystem()
    implicit val materializer = ActorMaterializer()

    // Load Config
    val config = ConfigFactory.load()

    val mixingAddress: String = UUID.randomUUID().toString
    val mixingProperties =
      MixingProperties(
        minTransactionPerDestinationAddress = 10,
        maxTransactionPerDestinationAddress = 20,
        minTransactionAmount = BigDecimal(0),
        maxTransactionAmount = BigDecimal(10),
        maxScale = 3,
        numberOfMixRequestTaskToSchedule = 10
      )

    val supervisorActor: ActorRef = actorSystem.actorOf(
      SupervisorActor.props(
        address = mixingAddress,
        mixingProperties = mixingProperties,
        delayBetweenMixing = 10 seconds,
        delayBetweenAllTransactionFetching = 10 seconds,
        accountManagerInitialSeed = 13,
        mixedTransactionGeneratorInitialSeed = 20,
        mockAPI = true,
        numberOfInstancePerActor = 1,
        apiClientConfig = config
      )
    )

    supervisorActor ! SupervisorActor.StartTheWorld

    try {
      while (true) {
        println(prompt)
        val line = StdIn.readLine()

        if (line == "quit") throw CompletedException

        val addresses = line.split(",")
        if (line == "") {
          println(s"You must specify empty addresses to mix into!\n$helpText")
        } else {
          val depositAddress = UUID.randomUUID()
          val now = LocalDateTime.now()
          supervisorActor ! NewRequestDispatcherActor.NewRequests(
            Seq(MixRequestCoordinate(depositAddress.toString, addresses)),
            now
          )
          println(
            s"You may now send Jobcoins to address $depositAddress. They will be mixed and sent to your destination addresses."
          )
        }
      }
    } catch {
      case CompletedException => println("Quitting...")
    } finally {
      actorSystem.terminate()
    }
  }

  val prompt: String =
    "Please enter a comma-separated list of new, unused Jobcoin addresses where your mixed Jobcoins will be sent."
  val helpText: String =
    """
      |Jobcoin Mixer
      |
      |Takes in at least one return address as parameters (where to send coins after mixing). Returns a deposit address to send coins to.
      |
      |Usage:
      |    run return_addresses...
    """.stripMargin
}
