package controllers


import javax.inject.Inject
import play.api.mvc.{AbstractController, ControllerComponents}
import repositories.{TrafficRepository, TrafficSubscriptionRepository}
import services.SlackService

import scala.concurrent.ExecutionContext

class HomeController @Inject()(cc: ControllerComponents, repository: TrafficSubscriptionRepository, slackService: SlackService)
  (implicit ec: ExecutionContext) extends AbstractController(cc) {

  def index = Action.async {
    repository.getAll().map(l => Ok(l.map(_.toString).mkString(", ")))
  }
}
