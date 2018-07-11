package actor

import akka.actor.{Actor, Props}
import com.lunatech.slack.client.models.ChatMessage
import javax.inject.Inject
import models.Status
import play.api.Logger
import repositories.TrafficRepository
import services.{RATPService, SlackService, TrainResultError, TrainResultSuccess}

import scala.concurrent.{ExecutionContext, Future}

class TrafficActor @Inject()(ratp: RATPService, repository: TrafficRepository, slackService: SlackService)(implicit ec: ExecutionContext) extends Actor {

  import actor.TrafficActor._

  override def receive: Receive = {
    case _: RER =>
      Logger.info("RER")
      loadTraffic("rers")
    case _: METRO =>
      Logger.info("METRO")
      loadTraffic("metros")
    case x =>
      Logger.info(s"erreur ${x.toString}")
  }

  private def loadTraffic(transport: String) = {
    Logger.debug(s"loading $transport")

    val serviceTraffic: Future[Seq[Status]] = ratp.getTraffics(transport) flatMap {
      case TrainResultSuccess(s) =>
        Future.successful(s)
      case TrainResultError(e) =>
        Logger.info(e.getMessage)
        Future.failed(e)
    }

    val dbTraffic: Future[Seq[Status]] = repository.listFromTransportType(transport)
      .map(l => l.map(t => Status(t.line, t.slug, t.title, t.message)))

    dbTraffic.map(status => if (status.isEmpty) {
      serviceTraffic.map(serviceStatus => serviceStatus.map(ss => repository.create(transport, ss)))
    })

    val differentFromDb: Future[Seq[Status]] = serviceTraffic.zip(dbTraffic).map {
      case (service, db) => for {
        s <- service
        d <- db
        if (s.line == d.line && s != d) || (!db.map(d => d.line).contains(s.line))
      } yield s
    }

    differentFromDb.map { s =>
      s.map { status =>
        repository.update(status)
        val attachment = slackService.getStatusAttachment(transport, status)
        slackService.slackClient.postMessage(ChatMessage("#rer", "", attachments = Some(Seq(attachment))))
      }
    }
  }
}

object TrafficActor {
  def props = Props[TrafficActor]

  case class RER()

  case class METRO()

}
