package com.gemini.jobcoin.actors

import java.time.LocalDateTime

import akka.actor.{ActorRef, Props}
import com.gemini.jobcoin.mixrequest.{MixRequest, MixRequestCoordinate}

case class NewRequestHandlerActor() extends MixerActor {

  import NewRequestHandlerActor._

  override def receive: Receive = idle

  def idle: Receive = {
    case StartWith(accountManagerActorRef) =>
      context.become(handle(accountManagerActorRef))
  }

  def handle(accountManagerActor: ActorRef): Receive = {
    case NewRequests(mixRequestCoordinates) =>
      val now = LocalDateTime.now()
      val mixRequests = mixRequestCoordinates.map(MixRequest(now, _))
      accountManagerActor ! AccountManagerActor.NewRequest(mixRequests)
  }

}

object NewRequestHandlerActor {
  def props: Props = Props(NewRequestHandlerActor())

  case class StartWith(accountManagerActorRef: ActorRef)

  case class NewRequests(mixRequestCoordinates: Seq[MixRequestCoordinate])

}