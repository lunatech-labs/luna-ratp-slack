package controllers


import com.lunatech.slack.client.Parser
import com.lunatech.slack.client.models._
import javax.inject.Inject
import models.TrainDestination
import play.api.libs.json.Json
import play.api.mvc._
import play.api.{Configuration, Logger}
import services._

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

class SlackController @Inject()(cc: ControllerComponents, config: Configuration, ratp: RATPService, slackService: SlackService)
  (implicit ec: ExecutionContext) extends AbstractController(cc) {


  implicit val messageFormat = Json.format[Message]

  def nextRER = Action { request =>
    Ok(Json.toJson(slackService.selectTransportMessage))
  }

  def suggestions = Action { request =>
    Logger.info(request.body.toString)
    Ok("")
  }

  def interactive = Action.async { request =>
    val payload = Parser.getPayload(request.body.asFormUrlEncoded.getOrElse(Map()))

    Logger.info(request.body.toString)

    payload match {
      case Success(s) => s.callback_id match {
        case "select_transport" => selectCode(s)
        case "select_code" => selectStation(s) recoverWith {
          case e: Exception => Future.successful(Ok(Json.toJson(slackService.errorMessage(e.getMessage))))
        }
        case "select_station" => showTrains(s) recoverWith {
          case e: Exception => Future.successful(Ok(Json.toJson(slackService.errorMessage(e.getMessage))))
        }
        case _ => Future.successful(Ok("Je ne sais pas résoudre cette action"))
      }
      case Failure(e) => Future.successful(Ok(Json.toJson(slackService.errorMessage(e.getMessage))))
    }
  }

  private def showTrains(payload: Payload) = {
    val actions = payload.actions

    val options = actions.flatMap(x => x.headOption.flatMap(x => x.selected_options))

    options match {
      case Some(opt) if opt.length == 1 =>
        val values = opt.head

        val params = values.value.split("_")

        val transport = params(0)
        val code = params(1)
        val station = params(2)

        nextRer(transport, code, station)
    }
  }

  private def selectCode(payload: Payload) = {
    val options = payload.actions.flatMap(x => x.headOption.flatMap(x => x.selected_options))
    (options match {
      case Some(opt) if opt.length == 1 =>
        val transport = opt.head

        Logger.info(transport.toString)

        slackService.selectCodeMessage(transport).map(message => Ok(Json.toJson(message)))
      case _ => Future.successful(Ok(Json.toJson(slackService.errorMessage("No train found"))))
    }) recoverWith {
      case e: Exception => Future.successful(Ok(Json.toJson(slackService.errorMessage(e.getMessage))))
    }
  }

  private def selectStation(payload: Payload) = {
    val options = payload.actions.flatMap(x => x.headOption.flatMap(x => x.selected_options))

    (options match {
      case Some(opt) if opt.length == 1 =>
        val transport = opt.head

        Logger.info(transport.toString)

        slackService.selectStationMessage(transport).map(message => Ok(Json.toJson(message)))
      case _ => Future.successful(Ok(Json.toJson(slackService.errorMessage("No train found"))))
    }) recoverWith {
      case e: Exception => Future.successful(Ok(Json.toJson(slackService.errorMessage(e.getMessage))))
    }
  }

  private def nextRer(transport: String, code: String, station: String) = {
    val destination = TrainDestination(transport, code, station)

    (ratp.nextTrain(destination) map {
      case TrainResultSuccess(s) => {
        val attachments = slackService.toAttachmentNextTrains(s, "trains", "trains")

        val message = Message(Some(s"_Voici les prochains *${transport.toUpperCase} $code* à *$station*_")).addAttachment(attachments)

        Ok(Json.toJson(message))
      }

      case TrainResultError(e) => Ok(
        Json.toJson(slackService.errorMessage(e.getMessage))
      )
    }) recoverWith {
      case e: Exception => Future.successful(Ok(Json.toJson(slackService.errorMessage(e.getMessage))))
    }
  }

}
