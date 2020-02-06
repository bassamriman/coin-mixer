package com.gemini.jobcoin.actors

import akka.actor.{ActorRef, PoisonPill, Props}
import com.gemini.jobcoin.common.MixerActor
import com.gemini.jobcoin.mixrequest.MixingProperties

import scala.concurrent.duration._

case class SupervisorActor(address: String,
                           mixingProperties: MixingProperties,
                           delayBetweenMixing: FiniteDuration,
                           delayBetweenAllTransactionFetching: FiniteDuration,
                           accountManagerInitialSeed: Long,
                           mixedTransactionGeneratorInitialSeed: Long,
                           mockAPI: Boolean,
                          ) extends MixerActor {

  import SupervisorActor._

  override def receive: Receive = idle

  def idle: Receive = {
    case SupervisorActor =>

      val balanceMonitorActor: ActorRef =
        context.actorOf(BalanceMonitorActor.props)

      val mixedTransactionGeneratorActor: ActorRef =
        context.actorOf(
          MixedTransactionGeneratorActor.props(
            mixingProperties,
            mixedTransactionGeneratorInitialSeed))

      val validatorActor: ActorRef =
        context.actorOf(ValidatorActor.props)

      val apiAccessActor: ActorRef =
        context.actorOf(if (mockAPI) MockAPIAccessActor.props else APIAccessActor.props)

      val committerActor: ActorRef =
        context.actorOf(CommitterActor.props(apiAccessActor = apiAccessActor))

      val ledgerActor: ActorRef =
        context.actorOf(LedgerActor.props(
          subscribers = Seq(balanceMonitorActor, validatorActor),
          apiAccessActor = apiAccessActor))

      val accountManagerActor: ActorRef =
        context.actorOf(
          AccountManagerActor.props(
            address = address,
            mixingProperties = mixingProperties,
            initialSeed = accountManagerInitialSeed,
            balanceMonitorActor = balanceMonitorActor,
            mixedTransactionGeneratorActor = mixedTransactionGeneratorActor,
            committerActor = committerActor,
            validatorActor = validatorActor
          )
        )

      val newRequestDispatcherActor: ActorRef =
        context.actorOf(NewRequestDispatcherActor.props(accountManagerActor))

      val evaluationContext = context.system.dispatcher
      context.system.scheduler.schedule(
        initialDelay = delayBetweenMixing,
        interval = delayBetweenMixing,
        receiver = accountManagerActor,
        message = AccountManagerActor.ScheduleNewMixRequestTasks)(evaluationContext)

      context.system.scheduler.schedule(
        initialDelay = delayBetweenAllTransactionFetching,
        interval = delayBetweenAllTransactionFetching,
        receiver = ledgerActor,
        message = LedgerActor.FetchLatestLedger)(evaluationContext)

      context.become(
        worldStarted(
          accountManagerActor = accountManagerActor,
          balanceMonitorActor = balanceMonitorActor,
          committerActor = committerActor,
          validatorActor = validatorActor,
          ledgerActor = ledgerActor,
          apiAccessActor = apiAccessActor,
          newRequestHandlerActor = newRequestDispatcherActor
        )
      )
  }

  def worldStarted(accountManagerActor: ActorRef,
                   balanceMonitorActor: ActorRef,
                   committerActor: ActorRef,
                   validatorActor: ActorRef,
                   ledgerActor: ActorRef,
                   apiAccessActor: ActorRef,
                   newRequestHandlerActor: ActorRef): Receive = {
    case EndTheWorld =>
      context.children.foreach(_ ! PoisonPill)
      context.system.terminate()
    case newMixRequest: NewRequestDispatcherActor.NewRequests =>
      newRequestHandlerActor ! newMixRequest
  }
}

object SupervisorActor {

  def props(address: String,
            mixingProperties: MixingProperties,
            delayBetweenMixing: FiniteDuration,
            delayBetweenAllTransactionFetching: FiniteDuration,
            accountManagerInitialSeed: Long,
            mixedTransactionGeneratorInitialSeed: Long,
            mockAPI: Boolean): Props =
    Props(SupervisorActor(
      address = address,
      mixingProperties = mixingProperties,
      delayBetweenMixing = delayBetweenMixing,
      delayBetweenAllTransactionFetching = delayBetweenAllTransactionFetching,
      accountManagerInitialSeed = accountManagerInitialSeed,
      mixedTransactionGeneratorInitialSeed = mixedTransactionGeneratorInitialSeed,
      mockAPI = mockAPI
    ))

  case object StartTheWorld

  case object EndTheWorld

}