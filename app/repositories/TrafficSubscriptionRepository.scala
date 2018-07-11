package repositories

import javax.inject.Inject
import models.{Traffic, TrafficSubscription}
import play.api.Logger
import play.api.db.slick.DatabaseConfigProvider
import slick.jdbc.JdbcProfile

import scala.concurrent.{ExecutionContext, Future}

class TrafficSubscriptionRepository @Inject()(dbConfigProvider: DatabaseConfigProvider)(implicit ec: ExecutionContext) {
  private val dbConfig = dbConfigProvider.get[JdbcProfile]

  import dbConfig._
  import profile.api._

  private class TrafficSubscriptionTable(tag: Tag) extends Table[TrafficSubscription](tag, "TRAFFIC_SUBSCRIPTION") {
    def userId = column[String]("USERID", O.PrimaryKey)
    def line = column[String]("LINE", O.PrimaryKey)

    def * = (userId, line) <> ((TrafficSubscription.apply _).tupled, TrafficSubscription.unapply)
  }

  private val subscriptions = TableQuery[TrafficSubscriptionTable]


  def create(subscription: TrafficSubscription): Future[Int] = db.run {
    Logger.info(s"created ${subscription.toString}")
    subscriptions += subscription
  }

  def getIdsByLine(line: String): Future[Seq[String]] = db.run {
    subscriptions.filter(_.line === line).map(_.userId).result
  }

  def getAll(): Future[Seq[TrafficSubscription]] = db.run {
    subscriptions.result
  }
}
