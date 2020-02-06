package com.gemini.jobcoin.common

import akka.actor.{Actor, ActorLogging}

trait MixerActor extends Actor with ActorLogging {
//  override protected def aroundReceive(receive: Receive, msg: Any): Unit = {
//    log.info(s"Actor: ${self.path.name} received message: $msg")
//    super.aroundReceive(receive, msg)
//  }
}
