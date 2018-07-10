package repositories

import javax.inject.{Inject, Singleton}
import models.{Status, Traffic}
import play.api.db.slick.DatabaseConfigProvider
import slick.jdbc.JdbcProfile

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class TrafficRepository @Inject()(dbConfigProvider: DatabaseConfigProvider)(implicit ec: ExecutionContext) {
  private val dbConfig = dbConfigProvider.get[JdbcProfile]

  import dbConfig._
  import profile.api._

  private class UserTable(tag: Tag) extends Table[Traffic](tag, "TRAFFIC") {
    def transport = column[String]("TRANSPORT")

    def line = column[String]("LINE", O.PrimaryKey)

    def slug = column[String]("SLUG")

    def title = column[String]("TITLE")

    def message = column[String]("MESSAGE")

    def * = (transport, line, slug, title, message) <> ((Traffic.apply _).tupled, Traffic.unapply)
  }

  private val traffics = TableQuery[UserTable]

  def create(transport: String, status: Status): Future[Int] = db.run {
    traffics += Traffic(transport, status.line, status.slug, status.title, status.message)
  }

  def list(): Future[Seq[Traffic]] = db.run {
    traffics.result
  }

  def listFromTransportType(transportType: String): Future[Seq[Traffic]] = db.run {
    traffics.filter(_.transport === transportType).result
  }
}

