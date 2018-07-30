package repositories

import java.time.DayOfWeek

import javax.inject.Inject
import models.AlertType.AlertType
import models.{AlertForm, AlertType, DayAlertForm}
import play.api.Logger
import play.api.db.slick.DatabaseConfigProvider
import slick.jdbc.JdbcProfile

import scala.concurrent.{ExecutionContext, Future}

class AlertFormRepository @Inject()(dbConfigProvider: DatabaseConfigProvider)(implicit ec: ExecutionContext) {

  private val dbConfig = dbConfigProvider.get[JdbcProfile]

  import dbConfig._
  import profile.api._

  private implicit val alertTypeMapper = MappedColumnType.base[AlertType, String](
    e => e.toString,
    s => AlertType.withName(s)
  )


  private class AlertFormTable(tag: Tag) extends Table[AlertForm](tag, "ALERTFORM") {

    def id = column[String]("ID", O.PrimaryKey)

    def userId = column[String]("USERID", O.PrimaryKey)

    def alertType = column[AlertType]("ALERTTYPE")

    def transportType = column[Option[String]]("TRANSPORTTYPE")

    def transportCode = column[Option[String]]("TRANSPORTCODE")

    def transportStation = column[Option[String]]("TRANSPORTSTATION")

    def alertDay = column[Int]("ALERTDAY")

    def hour = column[Option[Int]]("HOUR")

    def minutes = column[Option[Int]]("MINUTES")

    def * =
      (id, userId, alertDay, alertType, transportType, transportCode, transportStation, hour, minutes) <> ((AlertForm.apply _).tupled, AlertForm.unapply)
  }

  private val alerts = TableQuery[AlertFormTable]

  private class AlertDayForm(tag: Tag) extends Table[DayAlertForm](tag, "ALERTDAYFORM") {
    def alertId = column[String]("ID", O.PrimaryKey)

    def day = column[Int]("DAY", O.PrimaryKey)

    def * = (alertId, day) <> ((DayAlertForm.apply _).tupled, DayAlertForm.unapply)
  }

  private val days = TableQuery[AlertDayForm]

  def create(alertForm: AlertForm): Future[Int] = db.run {
    alerts += alertForm
  }

  def delete(id: String): Future[Int] = {
    db.run(
      days
        .filter(_.alertId === id)
        .delete
    ).flatMap { count =>
      db.run(
        alerts
          .filter(_.id === id)
          .delete
      ).map(x => x + count)
    }
  }

  def updateAlertType(id: String, alertType: AlertType) = db.run {
    alerts.filter(_.id === id).map(_.alertType).update(alertType)
  }

  def createOrDeleteDay(id: String, day: DayOfWeek): Future[Int] = {
    val query = days
      .filter(_.alertId === id)
      .filter(_.day === day.getValue)

    val deleteQuery = query.delete

    val addQuery = days += DayAlertForm(id, day.getValue)

    db.run(query.result).flatMap { res =>
      // Create if it doesn't exist
      if (res.isEmpty) {
        db.run(addQuery)
        // Delete if it exists
      } else {
        db.run(deleteQuery)
      }
    }
  }

  def updateAlertDay(id: String, alertDay: Int): Future[Int] = db.run {
    alerts.filter(_.id === id).map(_.alertDay).update(alertDay)
  }

  def getDaysForAlertForm(id: String): Future[Seq[DayOfWeek]] = {
    db.run(days
      .filter(_.alertId === id)
      .result).map(x => x.map(day => DayOfWeek.of(day.day)))
  }

  def updateType(id: String, `type`: String): Future[Int] = db.run {
    alerts
      .filter(_.id === id)
      .map(element => (element.transportType, element.transportCode, element.transportStation))
      .update(Some(`type`), None, None)
  } map { x =>
    Logger.info(x.toString)
    x
  }

  def updateCode(id: String, code: String): Future[Int] = db.run {
    alerts
      .filter(_.id === id)
      .map(element => (element.transportCode, element.transportStation))
      .update(Some(code), None)
  } map { x =>
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
