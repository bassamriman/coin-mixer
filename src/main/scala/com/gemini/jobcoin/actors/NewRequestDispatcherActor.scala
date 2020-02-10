package com.gemini.jobcoin.actors

import java.time.LocalDateTime

import akka.actor.{ActorRef, Props}
import com.gemini.jobcoin.common.MixerActor
import com.gemini.jobcoin.mixrequest.models.MixRequestCoordinate

/**
  * NewRequestDispatcherActor is the gateway to entire actor system.
  * Mix Requests will be forwarded to the accountAndMixRequestManagerActor
  * @param newRequestHandler
  */
case class NewRequestDispatcherActor(newRequestHandler: ActorRef)
    extends MixerActor {

  import NewRequestDispatcherActor._

  override def receive: Receive = logged(handle)

  def handle: Receive = {
    case msg: NewRequests =>
      newRequestHandler ! msg
  }

}

object NewRequestDispatcherActor {
  def props(newRequestHandler: ActorRef): Props =
    Props(NewRequestDispatcherActor(newRequestHandler))

  case class NewRequests(mixRequestCoordinates: Seq[MixRequestCoordinate],
                         timestamp: LocalDateTime)

}
