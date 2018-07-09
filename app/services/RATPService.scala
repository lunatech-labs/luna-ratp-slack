package services

import java.net.URI

import javax.inject.{Inject, Singleton}
import models.{Station, TrainDestination, TrainSchedule, Transportation}
import play.api.libs.json._
import play.api.libs.ws.WSClient
import play.api.{Configuration, Logger}

import scala.concurrent.{ExecutionContext, Future}

sealed trait TrainResult[T]

case class TrainResultSuccess[T](trains: T) extends TrainResult[T]

case class TrainResultError[T](error: Exception) extends TrainResult[T]

case class TrainCode(code: String, name: String)

@Singleton
class RATPService @Inject()(ws: WSClient, config: Configuration)(implicit ec: ExecutionContext) {

  def nextTrain(destination: TrainDestination): Future[TrainResult[Seq[TrainSchedule]]] = {
    val url = config.get[String]("ratp.api.base") + s"/schedules/${destination.trainType}/${destination.trainCode}/${destination.station}/A+R"

    Logger.info(url)

    ws.url(url.replaceAll("’", "'"))
      .get()
      .flatMap(response => {
        Logger.info(response.body)
        (response.json \ "result" \ "schedules").validate[Seq[TrainSchedule]] match {
          case JsSuccess(s, _) => Future.successful(TrainResultSuccess(s))
          case _ => Future.successful(getErrorMessage(response.json))
        }
      }
      )
  }


  def getCodes(transport: String): Future[TrainResult[Seq[TrainCode]]] = {
    val url = config.get[String]("ratp.api.base") + s"/lines/$transport"

    ws.url(url)
      .get()
      .flatMap(response => {
        (response.json \ "result" \ transport).validate[Seq[Transportation]] match {
          case JsSuccess(s, _) => Future.successful(TrainResultSuccess(s.map(train => TrainCode(train.code, train.name)).distinct))

          case _ => Future.successful(getErrorMessage(response.json))
        }
      })
  }

  def getStations(transport: String, code: String): Future[TrainResult[Seq[Station]]] = {
    val url = config.get[String]("ratp.api.base") + s"/stations/$transport/$code"

    ws.url(url)
      .get()
      .flatMap(response => {
        (response.json \ "result" \ "stations").validate[Seq[Station]] match {
          case JsSuccess(s, _) => Future.successful(TrainResultSuccess(s))
          case _ => Future.successful(getErrorMessage(response.json))
        }
      })
  }

  private def getErrorMessage[T](json: JsValue): TrainResultError[T] = {
    (json \ "result" \ "message").validate[String] match {
      case JsSuccess(s, _) => TrainResultError(new Exception(s"Error : $s"))
      case JsError(_) => TrainResultError(new Exception("Unexpected error"))
    }
  }
}
