package controllers


import javax.inject.Inject
import play.api.mvc.{AbstractController, ControllerComponents}
import repositories.{TrafficRepository, TrafficSubscriptionRepository}
import services.SlackService

import scala.concurrent.ExecutionContext

class HomeController @Inject()(cc: ControllerComponents, trafficRepo: TrafficRepository, repository: TrafficSubscriptionRepository, slackService: SlackService)
  (implicit ec: ExecutionContext) extends AbstractController(cc) {

  def index = Action.async {
    trafficRepo.update(models.Status("A", "adds", "asdasd", "asdads"))
    trafficRepo.list().map(l => Ok(l.map(_.toString).mkString(", ")))
  }
}
