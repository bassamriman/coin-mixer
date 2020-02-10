package com.gemini.jobcoin.common

import akka.actor.{Actor, ActorLogging}
import com.gemini.jobcoin.LogSettings

/**
  * Parent actor trait used for common logic
  */
trait MixerActor extends Actor with ActorLogging {

  /**
    * Log messages
    * @param pf1
    * @tparam A
    * @tparam Unit
    */
  def logged[A, Unit](
    pf1: PartialFunction[A, Unit]
  ): PartialFunction[A, Unit] = {
    val logPF: PartialFunction[A, A] = {
      case msg =>
        val actorName: String = context.self.path.name
        val sender: String = context.sender().path.name
        if (LogSettings.logMessages)
          log.info(s"From: $sender || To: $actorName || Msg: $msg")
        msg

    }
    andThenPartial(logPF, pf1)
  }

  private def andThenPartial[A, B, C](
    pf1: PartialFunction[A, B],
    pf2: PartialFunction[B, C]
  ): PartialFunction[A, C] = {
    Function.unlift(pf1.lift(_) flatMap pf2.lift)
  }
}
