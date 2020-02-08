package com.gemini.jobcoin.actors

import akka.actor.{ActorContext, ActorRef, Props}
import com.gemini.jobcoin.common.MixerActor

case class LoadBalancerActor(actorProps: Props,
                             name: String,
                             children: List[ActorRef])
    extends MixerActor {

  import LoadBalancerActor._

  override def receive: Receive = logged(handle(children))

  def handle(currentChildren: List[ActorRef]): Receive = {
    case Broadcast(msg) =>
      children.foreach(child => child ! msg)
    case msg =>
      currentChildren match {
        case currentChild :: remainingChildren =>
          currentChild.forward(msg)
          context.become(logged(handle(remainingChildren)))
        case Nil =>
          val currentChild :: remainingChildren = children
          currentChild.forward(msg)
          context.become(logged(handle(remainingChildren)))
      }
  }
}

object LoadBalancerActor {
  def props(actorProps: Props,
            name: String,
            numberOfInstances: Int,
            context: ActorContext): (Props, Seq[ActorRef]) = {
    val children: List[ActorRef] = (1 to numberOfInstances)
      .map(i => context.actorOf(actorProps, name + s"-$i"))
      .toList
    (Props(LoadBalancerActor(actorProps, name, children)), children)
  }

  case class Broadcast(msg: Any)

}
