package com.gemini.jobcoin.actors

import akka.actor.{ActorContext, ActorRef, Props}
import com.gemini.jobcoin.common.MixerActor

/**
  * LoadBalancerActor is responsible for distributing msg evenly amongst it's recipients
  * @param recipients are the actors to which the messages will be forwarded to
  */
case class LoadBalancerActor(recipients: List[ActorRef]) extends MixerActor {

  import LoadBalancerActor._

  override def receive: Receive = logged(handle(recipients))

  def handle(currentRecipients: List[ActorRef]): Receive = {
    case Broadcast(msg) =>
      recipients.foreach(recipient => recipient ! msg)
    case msg =>
      currentRecipients match {
        case currentReceipts :: remainingRecipients =>
          currentReceipts.forward(msg)
          context.become(logged(handle(remainingRecipients)))
        case Nil =>
          val currentRecipients :: remainingRecipients = recipients
          currentRecipients.forward(msg)
          context.become(logged(handle(remainingRecipients)))
      }
  }
}

object LoadBalancerActor {
  def props(actorProps: Props,
            name: String,
            numberOfInstances: Int,
            context: ActorContext): (Props, Seq[ActorRef]) = {
    val recipient: List[ActorRef] = (1 to numberOfInstances)
      .map(i => context.actorOf(actorProps, name + s"-$i"))
      .toList
    (Props(LoadBalancerActor(recipient)), recipient)
  }

  case class Broadcast(msg: Any)

}
