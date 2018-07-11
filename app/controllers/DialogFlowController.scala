package controllers

import com.lunatech.slack.client.api.SlackClient
import com.lunatech.slack.client.models.Message
import javax.inject.Inject
import models.{DialogFlowModel, TrainDestination}
import play.api.libs.json.Json
import play.api.mvc.{AbstractController, ControllerComponents}
import play.api.{Configuration, Logger}
import services.{RATPService, SlackService, TrainResultError, TrainResultSuccess}

import scala.concurrent.{ExecutionContext, Future}

class DialogFlowController @Inject()(cc: ControllerComponents, ratp: RATPService, slackService: SlackService)
  (implicit ec: ExecutionContext) extends AbstractController(cc) {


  def dialogFlow() = Action.async { request =>
    println(request.body.toString)
    val dialog = request.body.asJson.map(json => Json.fromJson[DialogFlowModel](json))

    val destination = request.body.asJson.map(json => Json.fromJson[DialogFlowModel](json)) match {
      case Some(v) => v.asOpt match {
        case Some(dialog) =>
          Logger.info(dialog.getIntent)
          Logger.info(dialog.getParameters("code"))
          Logger.info(dialog.getParameters("type"))
          Logger.info((dialog.getParameters("station").length == "val d'europe".length).toString)
          println(dialog.getParameters("station"))

          if (dialog.isAllParameterPresent) {
            Some(TrainDestination(trainCode = dialog.getParameters("code").trim, station = dialog.getParameters("station").trim, trainType = dialog.getParameters("type").trim))
          } else {
            None
          }

        case None =>
          None
      }
      case None => None
    }

    val messageFuture: Future[Message] = destination match {
      case Some(dest) => ratp.nextTrain(dest) map {
        case TrainResultSuccess(s) =>
          val attachments = slackService.toAttachmentNextTrains(s, "trains", "trains")

          Message(s"_Voici les prochains *${dest.trainType} ${dest.trainCode}* à *${dest.station}*_").addAttachment(attachments)

        case TrainResultError(e) => Message(e.getMessage)

      }
      case None => Future.successful(Message("Not a message from slack"))
    }

    messageFuture.map { message =>
      val json = Json.obj("payload" -> Json.obj("slack" -> message))
      Logger.info(json.toString())
      Ok(json)
    }
  }

}
