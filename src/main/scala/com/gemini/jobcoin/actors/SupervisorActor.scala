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
                           numberOfInstancePerActor: Int) extends MixerActor {

  import SupervisorActor._

  private val balanceMonitorActorName: String = "BalanceMonitorActor"
  private val mixedTransactionGeneratorActorName: String = "MixedTransactionGeneratorActor"
  private val validatorActorName: String = "ValidatorActor"
  private val apiAccessActorName: String = "ApiAccessActor"
  private val committerActorName: String = "CommitterActor"
  private val ledgerActorName: String = "LedgerActor"
  private val accountManagerActorName: String = "AccountManagerActor"
  private val newRequestDispatcherActorName: String = " NewRequestDispatcherActor"

  override def receive: Receive = idle

  def idle: Receive = {
    case SupervisorActor =>

      val (balanceMonitorLoadBalancerActor, balanceMonitorActors) =
        actorOfWithLoadBalancer(BalanceMonitorActor.props, balanceMonitorActorName)

      val (mixedTransactionGeneratorLoadBalancerActor, mixedTransactionGeneratorActors) =
        actorOfWithLoadBalancer(MixedTransactionGeneratorActor.props(
          mixingProperties,
          mixedTransactionGeneratorInitialSeed), mixedTransactionGeneratorActorName)

      val (validatorLoadBalancerActor, validatorActors) =
        actorOfWithLoadBalancer(ValidatorActor.props, validatorActorName)

      val (apiAccessActorLoadBalancerActor, apiAccessActors) =
        actorOfWithLoadBalancer(
          if (mockAPI) MockAPIAccessActor.props else APIAccessActor.props,
          apiAccessActorName)

      val (committerLoadBalancerActor, committerActors) =
        actorOfWithLoadBalancer(
          CommitterActor.props(apiAccessActorLoadBalancerActor),
          committerActorName)

      val ledgerActor: ActorRef =
        context.actorOf(LedgerActor.props(
          subscribers = balanceMonitorActors ++ validatorActors,
          apiAccessActor = apiAccessActorLoadBalancerActor),
          ledgerActorName)

      val (accountManagerLoadBalancerActor, accountManagerActors) =
        actorOfWithLoadBalancer(
          AccountManagerActor.props(
            address = address,
            mixingProperties = mixingProperties,
            initialSeed = accountManagerInitialSeed,
            balanceMonitorActor = balanceMonitorLoadBalancerActor,
            mixedTransactionGeneratorActor = mixedTransactionGeneratorLoadBalancerActor,
            committerActor = committerLoadBalancerActor,
            validatorActor = validatorLoadBalancerActor
          ), accountManagerActorName
        )

      val newRequestDispatcherActor: ActorRef =
        context.actorOf(NewRequestDispatcherActor.props(accountManagerLoadBalancerActor),
          newRequestDispatcherActorName)

      val evaluationContext = context.system.dispatcher

      context.system.scheduler.schedule(
        initialDelay = delayBetweenMixing,
        interval = delayBetweenMixing,
        receiver = accountManagerLoadBalancerActor,
        message = LoadBalancerActor.Broadcast(AccountManagerActor.ScheduleNewMixRequestTasks))(evaluationContext)

      context.system.scheduler.schedule(
        initialDelay = delayBetweenAllTransactionFetching,
        interval = delayBetweenAllTransactionFetching,
        receiver = ledgerActor,
        message = LedgerActor.FetchLatestLedger)(evaluationContext)

      context.become(worldStarted(newRequestHandlerActor = newRequestDispatcherActor))
  }

  def worldStarted(newRequestHandlerActor: ActorRef): Receive = {
    case EndTheWorld =>
      context.children.foreach(_ ! PoisonPill)
      context.system.terminate()
    case newMixRequest: NewRequestDispatcherActor.NewRequests =>
      newRequestHandlerActor ! newMixRequest
  }

  def actorOfWithLoadBalancer(props: Props, name: String): (ActorRef, Seq[ActorRef]) = {
    require(numberOfInstancePerActor >= 1, "Number of actor instance should be at least 1")
    val (loadBalancerProps, children) = LoadBalancerActor.props(
      props,
      name,
      numberOfInstancePerActor, context)
    (context.actorOf(loadBalancerProps, name + "-LoadBalancer"), children)
  }
}

object SupervisorActor {

  def props(address: String,
            mixingProperties: MixingProperties,
            delayBetweenMixing: FiniteDuration,
            delayBetweenAllTransactionFetching: FiniteDuration,
            accountManagerInitialSeed: Long,
            mixedTransactionGeneratorInitialSeed: Long,
            mockAPI: Boolean,
            numberOfInstancePerActor: Int): Props =
    Props(SupervisorActor(
      address = address,
      mixingProperties = mixingProperties,
      delayBetweenMixing = delayBetweenMixing,
      delayBetweenAllTransactionFetching = delayBetweenAllTransactionFetching,
      accountManagerInitialSeed = accountManagerInitialSeed,
      mixedTransactionGeneratorInitialSeed = mixedTransactionGeneratorInitialSeed,
      mockAPI = mockAPI,
      numberOfInstancePerActor
    ))

  case object StartTheWorld

  case object EndTheWorld

}