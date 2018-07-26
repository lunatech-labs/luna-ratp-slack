package repositories

import java.time.DayOfWeek

import javax.inject.Inject
import models.{Alert, DayAlert}
import play.api.db.slick.DatabaseConfigProvider
import slick.jdbc.JdbcProfile

import scala.concurrent.{ExecutionContext, Future}

class AlertRepository @Inject()(dbConfigProvider: DatabaseConfigProvider)(implicit ec: ExecutionContext) {
  private val dbConfig = dbConfigProvider.get[JdbcProfile]

  import dbConfig._
  import profile.api._

  private class AlertTable(tag: Tag) extends Table[Alert](tag, "ALERT") {
    def id = column[Int]("ID", O.PrimaryKey, O.AutoInc)

    def userId = column[String]("USERID")

    def trainType = column[String]("TRAINTYPE")

    def trainCode = column[String]("TRAINCODE")

    def station = column[String]("STATION")

    def hour = column[Int]("HOUR")

    def minutes = column[Int]("MINUTES")

    def * = (id, userId, trainType, trainCode, station, hour, minutes) <> ((Alert.apply _).tupled, Alert.unapply)
  }

  private val alerts = TableQuery[AlertTable]

  private class DayAlertTable(tag: Tag) extends Table[DayAlert](tag, "DAYALERT") {
    def id = column[Int]("ID", O.PrimaryKey, O.AutoInc)

    def alertId = column[Int]("ALERTID")

    def day = column[Int]("day")

    val alert = foreignKey("ALERT_FK", alertId, alerts)(_.id, onDelete = ForeignKeyAction.Cascade)

    def * = (id, alertId, day) <> ((DayAlert.apply _).tupled, DayAlert.unapply)
  }

  private val dayAlerts = TableQuery[DayAlertTable]


  def getAlertForUser(userId: String) = {
    val alertsFromDb: Future[Seq[Alert]] = db.run(
      alerts
        .filter(_.userId === userId)
        .result
    )

    alertsFromDb flatMap {list =>
      Future.sequence(list
        .map { alert =>
          val alerts: Future[Seq[DayAlert]] = db.run(dayAlerts.filter(_.alertId === alert.id).result)

          alerts.map(days => (alert, days))
        }
      )
    }
  }

  def create(alert: Alert, days: DayOfWeek*): Future[Int] = {
    val addedAlertId = db.run((alerts returning alerts.map(_.id)) += alert)
    addedAlertId flatMap (x => addDayToAlert(x, days: _*))
    addedAlertId
  }

  def addDayToAlert(alertId: Int, days: DayOfWeek*): Future[Int] =
    Future.sequence(
      days
        .map(day => DayAlert(0, alertId, day.getValue))
        .map(day => db.run(dayAlerts += day))
    )
      .map(_.sum)

  def getAlert(alertId: Int): Future[(Alert, Seq[DayOfWeek])] = {
    val alert: Future[Option[Alert]] = db.run(
      alerts
        .filter(_.id === alertId)
        .take(1)
        .result
        .headOption
    )

    val days = db.run(
      dayAlerts
        .filter(_.alertId === alertId)
        .result
    )

    alert.zip(days).flatMap {
      case (Some(a), d) => Future.successful((a, d.map(x => DayOfWeek.of(x.day))))
      case _ => Future.failed(new NoSuchElementException)
    }
  }

}
