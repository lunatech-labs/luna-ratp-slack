package actor

import akka.actor.{Actor, Props}
import javax.inject.Inject
import models.Status
import play.api.Logger
import repositories.TrafficRepository
import services.{RATPService, TrainResultError, TrainResultSuccess}

import scala.concurrent.{ExecutionContext, Future}

class TrafficActor @Inject()(ratp: RATPService, repository: TrafficRepository)(implicit ec: ExecutionContext) extends Actor {

  import actor.TrafficActor._

  override def receive: Receive = {
    case _: RER => loadTraffic("rers")
    case _: METRO => loadTraffic("metros")
  }

  private def loadTraffic(transport: String) = {
    val serviceTraffic: Future[Seq[Status]] = ratp.getTraffics(transport) flatMap {
      case TrainResultSuccess(s) => Future.successful(s)
      case TrainResultError(e) => Future.failed(e)
    }

    val dbTraffic: Future[Seq[Status]] = repository.listFromTransportType(transport)
      .map(l => l.map(t => Status(t.line, t.slug, t.title, t.message)))

    val differentFromDb: Future[Seq[Status]] = serviceTraffic.zip(dbTraffic).map {
      case (service, db) => for {
        s <- service
        d <- db
        if (s.line == d.line && s != d) || !db.contains(s)
      } yield s
    }

    differentFromDb.map(s =>
      s.foreach { status =>
        repository.create(transport, status)
        Logger.info(s"Updated $transport $status")
      }
    )
  }
}

object TrafficActor {
  def props = Props[TrafficActor]

  case class RER()

  case class METRO()

}
