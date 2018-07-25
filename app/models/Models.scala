package models

import java.time.LocalDateTime

import models.TypeOfAlert.TypeOfAlert
import play.api.libs.json.Json

case class TrainSchedule(code: Option[String], message: String, destination: String)

case class TrainDestination(trainType: String, trainCode: String, station: String)

case class Transportation(code: String, name: String, directions: String, id: String)

case class Station(slug: String, name: String)

case class Status(line: String, slug: String, title: String, message: String)

case class Traffic(transport: String, line: String, slug: String, title: String, message: String)

case class TrafficSubscription(userId: String, transport: String, line: String)

case class UserHomeStation(userId: String, trainType: String, trainCode: String, station: String)

case class Alert(id: Int, userId: String, trainType: String, trainCode: String, station: String, hour: Int, minutes: Int, day: Option[Int] = None, month: Option[Int] = None, year: Option[Int] = None)

case class DayAlert(id: Int, alertId: Int, day: Int)

case class DayAlertForm(alertId: String, day: Int)

object TypeOfAlert extends Enumeration {
  type TypeOfAlert = Value
  val TODAY = Value("TODAY")
  val THE = Value("THE")
  val REPEAT = Value("REPEAT")
}

case class AlertForm(
  id: String,
  userId: String,
  typeOfAlert: TypeOfAlert,
  transportType: Option[String] = None,
  transportCode: Option[String] = None,
  transportStation: Option[String] = None,
  day: Option[Int] = None,
  month: Option[Int] = None,
  year: Option[Int] = None,
  hour: Option[Int] = None,
  minutes: Option[Int] = None
)

object AlertForm {
  def getFormForTime(
    id: String,
    userId: String,
    time: LocalDateTime
  ): AlertForm = new AlertForm(id,
    userId,
    typeOfAlert = TypeOfAlert.THE,
    day = Some(time.getDayOfMonth),
    month = Some(time.getMonthValue),
    year = Some(time.getYear),
    hour = Some(time.getHour),
    minutes = Some(time.getMinute))
}

object DayAlert {
  implicit val format = Json.format[DayAlert]
}

object Alert {
  implicit val format = Json.format[Alert]
}

object Status {
  implicit val format = Json.format[Status]
}

object Traffic {
  implicit val format = Json.format[Traffic]
}

object Station {
  implicit val format = Json.format[Station]
}

object TrainSchedule {
  implicit val format = Json.format[TrainSchedule]
}

object Transportation {
  implicit val format = Json.format[Transportation]
}


