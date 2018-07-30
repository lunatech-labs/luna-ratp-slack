package models

import models.EventType.EventType
import play.api.Logger
import play.api.libs.json._


object EventType extends Enumeration {
  type EventType = Value
  val MESSAGE = Value("message")
  val NEW_USER = Value("team_join")
}

case class SlackEvent(token: String, `type`: EventType, user: String)

object SlackEvent {
  implicit def reads: Reads[SlackEvent] = json => {
    val token = (json \ "token").as[String]
    val `type` = EventType.withName((json \ "event" \ "type").as[String])

    Logger.debug(`type`.toString)

    val user =
      if (`type` == EventType.MESSAGE) {
        (json \ "event" \ "user").validate[String]
      } else {
        (json \ "event" \ "user" \ "id").validate[String]
      }

    user match {
      case JsSuccess(u, _) => JsSuccess(SlackEvent(token, `type`, u))
      case _ => JsError("Unknown event type")
    }

  }
}

