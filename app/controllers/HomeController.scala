package controllers


import java.time.DayOfWeek._

import com.lunatech.slack.client.models.ChatEphemeral
import javax.inject.Inject
import models.Alert
import play.api.Logger
import play.api.libs.json.Json
import play.api.mvc.{AbstractController, ControllerComponents}
import repositories.{AlertRepository, TrafficRepository, TrafficSubscriptionRepository}
import services.SlackService

import scala.concurrent.{ExecutionContext, Future}

class HomeController @Inject()(cc: ControllerComponents, trafficRepo: TrafficRepository, repository: TrafficSubscriptionRepository, slackService: SlackService, alertRepo: AlertRepository)
  (implicit ec: ExecutionContext) extends AbstractController(cc) {

  def index = Action.async {
    trafficRepo.update(models.Status("A", "adds", "asdasd", "asdads"))
    trafficRepo.list().map(l => Ok(l.map(_.toString).mkString(", ")))
  }

  def date = Action {
//    alertRepo.create(Alert(0, "AAAA", "RER", "A", "Val d'europe", 23, 59), MONDAY, TUESDAY, WEDNESDAY) onComplete {
//      s => Logger.info(s.toString)
//    }
//
//    alertRepo.getAlertForDay(MONDAY) map { res =>
//      Ok(Json.toJson(res))
//    }

    Ok
  }
}
