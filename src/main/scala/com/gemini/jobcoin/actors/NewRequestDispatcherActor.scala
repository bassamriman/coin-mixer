package com.gemini.jobcoin.actors

import java.time.LocalDateTime

import akka.actor.{ActorRef, Props}
import com.gemini.jobcoin.common.MixerActor
import com.gemini.jobcoin.mixrequest.MixRequestCoordinate

case class NewRequestDispatcherActor(newRequestHandler: ActorRef)
    extends MixerActor {

  import NewRequestDispatcherActor._

  override def receive: Receive = handle

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
