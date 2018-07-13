package repositories

import javax.inject.{Inject, Singleton}
import models.{Status, Traffic}
import play.Logger
import play.api.db.slick.DatabaseConfigProvider
import slick.jdbc.JdbcProfile

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class TrafficRepository @Inject()(dbConfigProvider: DatabaseConfigProvider)(implicit ec: ExecutionContext) {
  private val dbConfig = dbConfigProvider.get[JdbcProfile]

  import dbConfig._
  import profile.api._

  private class TrafficTable(tag: Tag) extends Table[Traffic](tag, "TRAFFIC") {
    def line = column[String]("LINE", O.PrimaryKey)

    def transport = column[String]("TRANSPORT")

    def slug = column[String]("SLUG")

    def title = column[String]("TITLE")

    def message = column[String]("MESSAGE")

    def * = (transport, line, slug, title, message) <> ((Traffic.apply _).tupled, Traffic.unapply)
  }

  private val traffics = TableQuery[TrafficTable]

  def create(transport: String, status: Status): Future[Int] = db.run {
    traffics += Traffic(transport, status.line, status.slug, status.title, status.message)
  }

  def update(status: Status) = db.run {
    Logger.info(s"updated $status")
    val s = for {c <- traffics if c.line === status.line} yield (c.slug, c.title, c.message)
    s.update((status.slug, status.title, status.message))
  }

  def list(): Future[Seq[Traffic]] = db.run {
    traffics.result
  }

  def listFromTransportType(transportType: String): Future[Seq[Traffic]] = db.run {
    traffics.filter(_.transport === transportType).result
  }
}

