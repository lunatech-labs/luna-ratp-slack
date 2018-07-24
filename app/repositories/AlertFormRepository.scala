package repositories

import java.time.LocalDate

import javax.inject.Inject
import models.TypeOfAlert.TypeOfAlert
import models.{AlertForm, TypeOfAlert}
import play.api.Logger
import play.api.db.slick.DatabaseConfigProvider
import slick.jdbc.JdbcProfile

import scala.concurrent.{ExecutionContext, Future}

class AlertFormRepository @Inject()(dbConfigProvider: DatabaseConfigProvider)(implicit ec: ExecutionContext) {

  private val dbConfig = dbConfigProvider.get[JdbcProfile]

  import dbConfig._
  import profile.api._

  private implicit val typeOfAlertEnumMapper: BaseColumnType[TypeOfAlert] = MappedColumnType.base[TypeOfAlert, String](
    e => e.toString,
    s => TypeOfAlert.withName(s)
  )

  private class AlertFormTable(tag: Tag) extends Table[AlertForm](tag, "ALERTFORM") {

    def id = column[String]("ID", O.PrimaryKey)

    def userId = column[String]("USERID", O.PrimaryKey)

    def transportType = column[Option[String]]("TRANSPORTTYPE")

    def transportCode = column[Option[String]]("TRANSPORTCODE")

    def transportStation = column[Option[String]]("TRANSPORTSTATION")

    def typeOfAlert = column[TypeOfAlert]("TYPEOFALERT")

    def day = column[Option[Int]]("DAY")

    def month = column[Option[Int]]("MONTH")

    def year = column[Option[Int]]("YEAR")

    def hour = column[Option[Int]]("HOUR")

    def minutes = column[Option[Int]]("MINUTES")

    def * =
      (id, userId, typeOfAlert, transportType, transportCode, transportStation, day, month, year, hour, minutes) <> ((AlertForm.apply _).tupled, AlertForm.unapply)
  }

  private val alerts = TableQuery[AlertFormTable]

  def create(alertForm: AlertForm): Future[Int] = db.run {
    alerts += alertForm
  }

  def updateAlertType(id: String, typeOfAlert: TypeOfAlert): Future[Int] = db.run {
    alerts.filter(_.id === id).map(_.typeOfAlert).update(typeOfAlert)
  }

  def updateDay(id: String, day: Int): Future[Int] = db.run {
    alerts.filter(_.id === id).map(_.day).update(Some(day))
  }

  def updateMonth(id: String, month: Int): Future[Int] = {
    val alert = getAlertForm(id)

    alert.flatMap { a =>
      val year = a.year.getOrElse(LocalDate.now.getYear)
      val dayInMonth = LocalDate.of(year, month, 1).lengthOfMonth

      db.run {
        alerts
          .filter(_.id === id)
          .map(x => (x.day, x.month))
          .update((Some(Math.min(a.day.getOrElse(1), dayInMonth)), Some(month)))
      }
    }
  }

  def updateYear(id: String, year: Int): Future[Int] = {
    val alert = getAlertForm(id)
    val nbDay = LocalDate.of(year, 1, 1).lengthOfYear()

    alert.flatMap { a =>
      val month = a.month.getOrElse(1)
      val day =
        if (month == 2 && nbDay == 365) {
          Math.min(28, a.day.getOrElse(1))
        } else {
          a.day.getOrElse(1)
        }

      db.run {
        alerts
          .filter(_.id === id)
          .map(x => (x.day, x.year))
          .update(Some(day), Some(year))
      }
    }
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
