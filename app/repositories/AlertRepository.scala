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


  def create(alert: Alert, days: DayOfWeek*): Future[Int] = {
    val addedAlert = db.run((alerts returning alerts.map(_.id)) += alert)
    addedAlert flatMap (x => addDayToAlert(x, days: _*))
  }


  def addDayToAlert(alertId: Int, days: DayOfWeek*): Future[Int] =
    Future.sequence(
      days
        .map(day => DayAlert(0, alertId, day.getValue))
        .map(day => db.run(dayAlerts += day))
    )
      .map(_.sum)

  def getAlertForDay(day: DayOfWeek): Future[Seq[(Alert, DayAlert)]] = {
    val query = for {
      d <- dayAlerts if d.day === day.getValue
      a <- alerts if d.alertId === a.id
    } yield (a, d)

    db.run(query.result)
  }

}
