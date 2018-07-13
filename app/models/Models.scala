package models

import play.api.libs.json.Json


case class TrainSchedule(code: Option[String], message: String, destination: String)
case class TrainDestination(trainType: String, trainCode: String, station: String)
case class Transportation(code: String, name: String, directions: String, id: String)
case class Station(slug: String, name: String)

case class Status(line: String, slug: String, title: String, message: String)

case class Traffic(transport: String, line: String, slug: String, title: String, message: String)

case class TrafficSubscription(userId: String, transport: String, line: String)


object Status {
  implicit val format = Json.format[Status]
}

object Traffic {
  implicit val format = Json.format[Traffic]
}

object Station {
  implicit val format = Json.format[Station]
}

object TrainSchedule{
  implicit val format = Json.format[TrainSchedule]
}

object Transportation {
  implicit val format = Json.format[Transportation]
}


