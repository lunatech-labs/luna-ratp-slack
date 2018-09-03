package repositories

import javax.inject.Inject
import models.WelcomeAlertHour
import play.api.db.slick.DatabaseConfigProvider
import repositories.Schema._
import slick.jdbc.JdbcProfile

import scala.concurrent.{ExecutionContext, Future}

class WelcomeAlertHourRepository @Inject()(dbConfigProvider: DatabaseConfigProvider)(implicit ec: ExecutionContext) {
  private val dbConfig = dbConfigProvider.get[JdbcProfile]

  import dbConfig._
  import profile.api._

  private class WelcomeAlertHourTable(tag: Tag)(implicit s: Schema) extends Table[WelcomeAlertHour](tag, s,"welcomealerthour") {

    val userId = column[String]("USERID", O.PrimaryKey)
    val hour = column[Option[Int]]("HOUR")
    val minute = column[Option[Int]]("MINUTE")

    def * = (userId, hour, minute) <> (WelcomeAlertHour.tupled, WelcomeAlertHour.unapply)
  }

  private val alerts = TableQuery[WelcomeAlertHourTable]

  def create(userId: String): Future[Int] = db.run {
    alerts += WelcomeAlertHour(userId)
  }

  def changeHour(userId: String, hour: Int): Future[Int] = db.run {
    alerts.filter(_.userId === userId).map(_.hour).update(Some(hour))
  }

  def changeMinute(userId: String, minute: Int): Future[Int] = db.run {
    alerts.filter(_.userId === userId).map(_.minute).update(Some(minute))
  }

  def getAlert(userId: String):Future[Option[WelcomeAlertHour]] = db.run {
    alerts.filter(_.userId === userId).result
  } map (_.headOption)


}
