package controllers


import javax.inject.Inject
import play.api.mvc.{AbstractController, ControllerComponents}
import repositories.TrafficRepository
import services.SlackService

import scala.concurrent.ExecutionContext

class HomeController @Inject()(cc: ControllerComponents, repository: TrafficRepository, slackService: SlackService)
  (implicit ec: ExecutionContext) extends AbstractController(cc) {

  def index = Action {
    Ok
  }
}
