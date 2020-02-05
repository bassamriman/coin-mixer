package com.gemini.jobcoin.actors

import akka.actor.{ActorRef, PoisonPill, Props}
import com.gemini.jobcoin.mixrequest.MixingProperties

import scala.concurrent.duration._

case class SupervisorActor(address: String,
                           mixingProperties: MixingProperties,
                           delayBetweenMixing: FiniteDuration,
                           delayBetweenAllTransactionFetching: FiniteDuration,
                           mockAPI: Boolean) extends MixerActor {

  import SupervisorActor._

  override def receive: Receive = idle

  def idle: Receive = {
    case SupervisorActor =>

      val newRequestHandlerActor: ActorRef =
        context.actorOf(NewRequestHandlerActor.props)
      val balanceMonitorActor: ActorRef =
        context.actorOf(BalanceMonitorActor.props)
      val committerActor: ActorRef =
        context.actorOf(CommitterActor.props)
      val validatorActor: ActorRef =
        context.actorOf(ValidatorActor.props)
      val ledgerActor: ActorRef =
        context.actorOf(LedgerActor.props(Seq(balanceMonitorActor, validatorActor)))
      val apiAccessActor: ActorRef =
        context.actorOf(if (mockAPI) MockAPIAccessActor.props else APIAccessActor.props)
      val accountManagerActor: ActorRef =
        context.actorOf(
          AccountManagerActor.props(
            address = address,
            mixingProperties = mixingProperties)
        )

      ledgerActor ! LedgerActor.StartWith(apiAccessActor)
      newRequestHandlerActor ! NewRequestHandlerActor.StartWith(accountManagerActor)
      balanceMonitorActor ! BalanceMonitorActor.StartWith(accountManagerActor)
      committerActor ! CommitterActor.StartWith(apiAccessActor, accountManagerActor)
      validatorActor ! ValidatorActor.StartWith(accountManagerActor)
      accountManagerActor ! AccountManagerActor.StartWith(
        balanceMonitorActor = balanceMonitorActor,
        committerActor = committerActor,
        validatorActor = validatorActor
      )

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
          newRequestHandlerActor = newRequestHandlerActor
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
    case newMixRequest: NewRequestHandlerActor.NewRequests =>
      newRequestHandlerActor ! newMixRequest
  }
}

object SupervisorActor {

  def props(address: String,
            mixingProperties: MixingProperties,
            delayBetweenMixing: FiniteDuration,
            delayBetweenAllTransactionFetching: FiniteDuration,
            mockAPI: Boolean): Props =
    Props(SupervisorActor(
      address = address,
      mixingProperties = mixingProperties,
      delayBetweenMixing = delayBetweenMixing,
      delayBetweenAllTransactionFetching = delayBetweenAllTransactionFetching,
      mockAPI = mockAPI
    ))

  case object StartTheWorld

  case object EndTheWorld

}