package controllers

import com.lunatech.slack.client.models.Message
import javax.inject.Inject
import models.{DialogFlowModel, TrainDestination}
import play.api.Logger
import play.api.libs.json.Json
import play.api.mvc.{AbstractController, Codec, ControllerComponents}
import services.{RATPService, SlackService, TrainResultError, TrainResultSuccess}

import scala.concurrent.{ExecutionContext, Future}

class DialogFlowController @Inject()(cc: ControllerComponents, ratp: RATPService, slackService: SlackService)
  (implicit ec: ExecutionContext) extends AbstractController(cc) {


  implicit val codec = Codec.javaSupported("utf-8")

  def dialogFlow() = Action.async { request =>
    println(request.body.toString)
    val dialogOpt = request.body.asJson.map(json => Json.fromJson[DialogFlowModel](json))

    val destination = dialogOpt match {
      case Some(v) => v.asOpt match {
        case Some(dialog) =>

          for {
            trainType <- dialog.getParameters.get("type")
            code <- dialog.getParameters.get("code")
            station <- dialog.getParameters.get("station")
          } yield TrainDestination(
            trainType.trim.replaceAll("’", "'"),
            code.trim.replaceAll("’", "'"),
            station.trim.replaceAll("’", "'")
          )

        case None =>
          None
      }
      case None => None
    }

    val messageFuture: Future[Message] = destination match {
      case Some(dest) => ratp.nextTrain(dest) map {
        case TrainResultSuccess(s) =>
          val attachments = slackService.toAttachmentNextTrains(s, "trains", "trains")

          Message(s"_Voici les prochains *${ratp.nameOfType(dest.trainType)} ${dest.trainCode}* à *${dest.station}*_").addAttachment(attachments)

        case TrainResultError(e) => Message(e.getMessage)

      }
      case None => Future.successful(Message("Not a message from slack"))
    }

    messageFuture.map { message =>
      val json = Json.obj("payload" -> Json.obj("slack" -> message))
      Logger.info(json.toString())
      Ok(json).as(TEXT)
    }
  }

}
