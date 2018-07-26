package controllers


import javax.inject.Inject
import models.AlertForm
import play.api.Logger
import play.api.mvc.{AbstractController, ControllerComponents}
import repositories.{AlertRepository, TrafficRepository, TrafficSubscriptionRepository}
import services.SlackService

import scala.concurrent.ExecutionContext

class HomeController @Inject()(cc: ControllerComponents, trafficRepo: TrafficRepository, repository: TrafficSubscriptionRepository, slackService: SlackService, alertRepo: AlertRepository)
  (implicit ec: ExecutionContext) extends AbstractController(cc) {

  def index = Action.async {
    trafficRepo.update(models.Status("A", "adds", "asdasd", "asdads"))
    trafficRepo.list().map(l => Ok(l.map(_.toString).mkString(", ")))
  }

  def date = Action.async {
    //    alertRepo.create(Alert(0, "AAAA", "RER", "A", "Val d'europe", 23, 59), MONDAY, TUESDAY, WEDNESDAY) onComplete {
    //      s => Logger.info(s.toString)
    //    }
    //
    //    alertRepo.getAlertForDay(MONDAY) map { res =>
    //      Ok(Json.toJson(res))
    //    }

    alertRepo.getAlert(1).map(x => Logger.info(x.toString)).map(_ => Ok)

  }
}
