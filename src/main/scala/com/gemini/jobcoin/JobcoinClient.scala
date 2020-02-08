package com.gemini.jobcoin

import akka.stream.Materializer
import com.gemini.jobcoin.accounting.BasicTransaction
import com.gemini.jobcoin.accounting.BasicTransaction._
import com.gemini.jobcoin.dao.TransactionDAO
import com.gemini.jobcoin.dao.TransactionDAO._
import com.typesafe.config.Config
import play.api.libs.json._
import play.api.libs.ws.JsonBodyReadables._
import play.api.libs.ws.JsonBodyWritables._
import play.api.libs.ws.ahc._

import scala.async.Async._
import scala.concurrent.ExecutionContext.Implicits._
import scala.concurrent.Future

class JobcoinClient(config: Config)(implicit materializer: Materializer) {
  private val wsClient = StandaloneAhcWSClient()

  def getTransactions: Future[Seq[TransactionDAO]] = async {
    val response = await {
      wsClient
        .url("https://jobcoin.gemini.com/blast-isolating/api/transactions")
        .get()
    }

    response
      .body[JsValue]
      .validate[Seq[TransactionDAO]]
      .get
  }

  def postTransaction(transaction : BasicTransaction): Future[Option[String]] = async {
    await {
      wsClient
        .url("https://jobcoin.gemini.com/blast-isolating/api/transactions")
        .addHttpHeaders("Content-Type" -> "application/json", "Accept" -> "application/json")
        .post(Json.toJson(transaction))
        .map{
          response =>
            response.status match {
              case 200 => None
              case _ => Some(response.body)
            }
        }
    }
  }

}


