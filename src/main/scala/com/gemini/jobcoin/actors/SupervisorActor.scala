package com.gemini.jobcoin.actors

import akka.actor.{ActorRef, PoisonPill, Props}
import akka.stream.Materializer
import com.gemini.jobcoin.JobcoinClient
import com.gemini.jobcoin.common.MixerActor
import com.gemini.jobcoin.mixrequest.models.MixingProperties
import com.typesafe.config.Config

import scala.concurrent.duration._

/**
  * Parameters used of tuning performance
  * @param accountAndMixRequestManagerActorInitialSeed
  * @param mixedTransactionGeneratorInitialSeed
  * @param delayBetweenMixing the time we wait between each scheduling of new mixRequestTasks.
  *                           Useful to control the randomness whichtransactions are mixed in to
  *                           the destination addresses.
  *                           (A future improvement would be to make this random)
  * @param delayBetweenAllTransactionFetching is the delay between each time we fetch the jobcoin api
  *                                           for latest ledger
  * @param numberOfInstancePerActor this used for scaling. It's number of instance of actor
  *                                 we are going to create per actor type.
  *                                 (A future improvement would be to have one config per actor type).
  * @param numberOfMixRequestTaskToSchedule number of mix request tasks to schedule per each scheduling
  *                                         mix request tasks.
  *
  */
case class RunConfiguration(accountAndMixRequestManagerActorInitialSeed: Long,
                            mixedTransactionGeneratorInitialSeed: Long,
                            delayBetweenMixing: FiniteDuration,
                            delayBetweenAllTransactionFetching: FiniteDuration,
                            numberOfInstancePerActor: Int,
                            numberOfMixRequestTaskToSchedule: Int)

/**
  * The actor that is responsible for wiring all the actors together.
  *
  * @param mixingProperties
  * @param settings
  * @param apiClientConfig
  * @param materializer
  */
case class SupervisorActor(
  mixingProperties: MixingProperties,
  settings: RunConfiguration,
  apiClientConfig: Config
)(implicit materializer: Materializer)
    extends MixerActor {

  import SupervisorActor._

  private val balanceMonitorActorName: String = "BalanceMonitorActor"
  private val mixedTransactionGeneratorActorName: String =
    "MixedTransactionGeneratorActor"
  private val validatorActorName: String = "ValidatorActor"
  private val apiAccessActorName: String = "ApiAccessActor"
  private val committerActorName: String = "CommitterActor"
  private val ledgerPublisherActorName: String = "LedgerPublisherActor"
  private val accountAndMixRequestManagerActorName: String =
    "AccountAndMixRequestManagerActor"
  private val newRequestDispatcherActorName: String =
    "NewRequestDispatcherActor"

  override def receive: Receive = logged(idle)

  def idle: Receive = {
    case SupervisorActor.StartTheWorld =>
      val (balanceMonitorLoadBalancerActor, balanceMonitorActors) =
        actorOfWithLoadBalancer(
          BalanceMonitorActor.props,
          balanceMonitorActorName
        )

      val (
        mixedTransactionGeneratorLoadBalancerActor,
        mixedTransactionGeneratorActors
      ) =
        actorOfWithLoadBalancer(
          MixedTransactionGeneratorActor
            .props(
              mixingProperties,
              settings.mixedTransactionGeneratorInitialSeed
            ),
          mixedTransactionGeneratorActorName
        )

      val (validatorLoadBalancerActor, validatorActors) =
        actorOfWithLoadBalancer(ValidatorActor.props, validatorActorName)

      val (apiAccessActorLoadBalancerActor, apiAccessActors) =
        actorOfWithLoadBalancer(
          APIAccessActor.props(JobcoinClient(apiClientConfig)),
          apiAccessActorName
        )

      val (committerLoadBalancerActor, committerActors) =
        actorOfWithLoadBalancer(
          CommitterActor.props(apiAccessActorLoadBalancerActor),
          committerActorName
        )

      val ledgerPublisherActor: ActorRef =
        context.actorOf(
          LedgerPublisherActor.props(
            subscribers = balanceMonitorActors ++ validatorActors,
            apiAccessActor = apiAccessActorLoadBalancerActor
          ),
          ledgerPublisherActorName
        )

      val (accountManagerLoadBalancerActor, accountManagerActors) =
        actorOfWithLoadBalancer(
          AccountAndMixRequestManagerActor.props(
            mixingProperties = mixingProperties,
            numberOfMixRequestTaskToSchedule =
              settings.numberOfMixRequestTaskToSchedule,
            initialSeed = settings.accountAndMixRequestManagerActorInitialSeed,
            balanceMonitorActor = balanceMonitorLoadBalancerActor,
            mixedTransactionGeneratorActor =
              mixedTransactionGeneratorLoadBalancerActor,
            committerActor = committerLoadBalancerActor,
            validatorActor = validatorLoadBalancerActor
          ),
          accountAndMixRequestManagerActorName
        )

      val newRequestDispatcherActor: ActorRef =
        context.actorOf(
          NewRequestDispatcherActor.props(accountManagerLoadBalancerActor),
          newRequestDispatcherActorName
        )

      val evaluationContext = context.system.dispatcher

      context.system.scheduler.schedule(
        initialDelay = settings.delayBetweenMixing,
        interval = settings.delayBetweenMixing,
        receiver = accountManagerLoadBalancerActor,
        message = LoadBalancerActor.Broadcast(
          AccountAndMixRequestManagerActor.ScheduleNewMixRequestTasks
        )
      )(evaluationContext)

      context.system.scheduler.schedule(
        initialDelay = settings.delayBetweenAllTransactionFetching,
        interval = settings.delayBetweenAllTransactionFetching,
        receiver = ledgerPublisherActor,
        message = LedgerPublisherActor.FetchLatestLedger
      )(evaluationContext)

      context.become(
        logged(worldStarted(newRequestHandlerActor = newRequestDispatcherActor))
      )
  }

  def worldStarted(newRequestHandlerActor: ActorRef): Receive = {
    case EndTheWorld =>
      context.children.foreach(_ ! PoisonPill)
      context.system.terminate()
    case newMixRequest: NewRequestDispatcherActor.NewRequests =>
      newRequestHandlerActor ! newMixRequest
  }

  def actorOfWithLoadBalancer(props: Props,
                              name: String): (ActorRef, Seq[ActorRef]) = {
    require(
      settings.numberOfInstancePerActor >= 1,
      "Number of actor instance should be at least 1"
    )
    val (loadBalancerProps, children) =
      LoadBalancerActor.props(
        props,
        name,
        settings.numberOfInstancePerActor,
        context
      )
    (context.actorOf(loadBalancerProps, name + "-LoadBalancer"), children)
  }
}

object SupervisorActor {

  def props(
    mixingProperties: MixingProperties,
    settings: RunConfiguration,
    apiClientConfig: Config
  )(implicit materializer: Materializer): Props =
    Props(
      SupervisorActor(
        mixingProperties = mixingProperties,
        settings = settings,
        apiClientConfig = apiClientConfig
      )
    )

  case object StartTheWorld

  case object EndTheWorld

}
