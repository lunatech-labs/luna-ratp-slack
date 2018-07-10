package repositories

import javax.inject.{Inject, Singleton}
import models.Traffic
import play.api.db.slick.DatabaseConfigProvider
import slick.jdbc.JdbcProfile

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class TrafficRepository @Inject()(dbConfigProvider: DatabaseConfigProvider)(implicit ec: ExecutionContext) {
  private val dbConfig = dbConfigProvider.get[JdbcProfile]

  import dbConfig._
  import profile.api._

  private class UserTable(tag: Tag) extends Table[Traffic](tag, "TRAFFIC") {
    def id = column[Int]("ID", O.PrimaryKey, O.AutoInc) // This is the primary key column
    def transport = column[String]("TRANSPORT")

    def line = column[String]("LINE")

    def slug = column[String]("SLUG")

    def title = column[String]("TITLE")

    def message = column[String]("MESSAGE")

    def * = (id, transport, line, slug, title, message) <> ((Traffic.apply _).tupled, Traffic.unapply)
  }

  private val traffics = TableQuery[UserTable]

  def create(traffic: Traffic): Future[Int] = db.run {
    traffics += traffic
  }

  def list(): Future[Seq[Traffic]] = db.run {
    traffics.result
  }

  def listFromTransportType(transportType: String): Future[Seq[Traffic]] = db.run {
    traffics.filter(_.transport === transportType).result
  }
}

