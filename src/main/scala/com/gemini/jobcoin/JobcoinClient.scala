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

trait JobcoinClient {
  def getTransactions: Future[Seq[BasicTransaction]]
  def postTransaction(transaction: BasicTransaction): Future[Option[String]]
}

class JobcoinClientImpl(config: Config)(implicit materializer: Materializer)
    extends JobcoinClient {
  private val wsClient = StandaloneAhcWSClient()
  private val url =
    "https://jobcoin.gemini.com/blast-isolating/api/transactions"

  def getTransactions: Future[Seq[BasicTransaction]] =
    async {
      val response = await {
        wsClient
          .url(url)
          .get()
      }

      response
        .body[JsValue]
        .validate[Seq[TransactionDAO]]
        .get
    }.map(_.map(BasicTransaction.apply))

  def postTransaction(transaction: BasicTransaction): Future[Option[String]] =
    async {
      await {
        wsClient
          .url(url)
          .addHttpHeaders(
            "Content-Type" -> "application/json",
            "Accept" -> "application/json"
          )
          .post(Json.toJson(transaction))
          .map { response =>
            response.status match {
              case 200 => None
              case _   => Some(response.body)
            }
          }
      }
    }
}

object JobcoinClient {
  def apply(config: Config)(
    implicit materializer: Materializer
  ): JobcoinClient = new JobcoinClientImpl(config)
}
