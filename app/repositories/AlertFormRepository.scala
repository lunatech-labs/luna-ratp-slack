package repositories

import javax.inject.Inject
import models.AlertForm
import play.api.Logger
import play.api.db.slick.DatabaseConfigProvider
import slick.jdbc.JdbcProfile

import scala.concurrent.{ExecutionContext, Future}

class AlertFormRepository @Inject()(dbConfigProvider: DatabaseConfigProvider)(implicit ec: ExecutionContext) {

  private val dbConfig = dbConfigProvider.get[JdbcProfile]

  import dbConfig._
  import profile.api._

  private class AlertFormTable(tag: Tag) extends Table[AlertForm](tag, "ALERTFORM") {

    def id = column[String]("ID", O.PrimaryKey)

    def userId = column[String]("USERID", O.PrimaryKey)

    def transportType = column[Option[String]]("TRANSPORTTYPE")

    def transportCode = column[Option[String]]("TRANSPORTCODE")

    def transportStation = column[Option[String]]("TRANSPORTSTATION")

    def hour = column[Option[Int]]("HOUR")

    def minutes = column[Option[Int]]("MINUTES")

    def * =
      (id, userId, transportType, transportCode, transportStation, hour, minutes) <> ((AlertForm.apply _).tupled, AlertForm.unapply)

  }

  private val alerts = TableQuery[AlertFormTable]

  def create(alertForm: AlertForm): Future[Int] = db.run {
    alerts += alertForm
  }

  def updateType(id: String, `type`: String): Future[Int] = db.run {
    alerts
      .filter(_.id === id)
      .map(element => (element.transportType, element.transportCode, element.transportStation))
      .update(Some(`type`), None, None)
  } map {x =>
    Logger.info(x.toString)
    x
  }

  def updateCode(id: String, code: String): Future[Int] = db.run {
    alerts
      .filter(_.id === id)
      .map(element => (element.transportCode, element.transportStation))
      .update(Some(code), None)
  } map {x =>
    Logger.info(x.toString)
    x
  }

  def updateStation(id: String, station: String): Future[Int] = db.run {
    alerts
      .filter(_.id === id)
      .map(element => element.transportStation)
      .update(Some(station))
  }

  def updateHour(id: String, hour: Int): Future[Int] = db.run {
    alerts
      .filter(_.id === id)
      .map(element => element.hour)
      .update(Some(hour))
  }

  def updateMinutes(id: String, minutes: Int): Future[Int] = db.run {
    alerts
      .filter(_.id === id)
      .map(element => element.minutes)
      .update(Some(minutes))
  }

  def getAlertForm(id: String): Future[AlertForm] = db.run {
    alerts
      .filter(_.id === id)
      .result
      .headOption
  } flatMap {
    case Some(s) => Future.successful(s)
    case _ => Future.failed(new NoSuchElementException("Invalid form"))
  }
}
