package models

import java.time.LocalDateTime

import models.AlertType.AlertType
import play.api.libs.json.Json

case class TrainSchedule(code: Option[String], message: String, destination: String)

case class TrainDestination(trainType: String, trainCode: String, station: String)

case class Transportation(code: String, name: String, directions: String, id: String)

case class Station(slug: String, name: String)

case class Status(line: String, slug: String, title: String, message: String)

case class Traffic(transport: String, line: String, slug: String, title: String, message: String)

case class TrafficSubscription(userId: String, transport: String, line: String)

case class UserHomeStation(userId: String, trainType: String, trainCode: String, station: String)

case class Alert(id: Int, userId: String, trainType: String, trainCode: String, station: String, hour: Int, minutes: Int)

case class DayAlert(id: Int, alertId: Int, day: Int)

case class DayAlertForm(alertId: String, day: Int)

case class AlertWithDays(alert: Alert, days: Option[Seq[DayAlert]])

case class WelcomeAlertHour(user: String, hour: Option[Int] = None, minute: Option[Int] = None)

case class AlertForm(
  id: String,
  userId: String,
  alertDay: Int,
  alertType: AlertType,
  transportType: Option[String] = None,
  transportCode: Option[String] = None,
  transportStation: Option[String] = None,
  hour: Option[Int] = None,
  minutes: Option[Int] = None
)

object AlertType extends Enumeration {
  type AlertType = Value
  val PONCTUAL = Value("Ponctuelle")
  val REPEAT = Value("Répétée")
}

object AlertForm {
  def getFormForTime(
    id: String,
    userId: String,
    time: LocalDateTime
  ): AlertForm = new AlertForm(id,
    userId,
    alertDay = 0,
    alertType = AlertType.PONCTUAL,
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


