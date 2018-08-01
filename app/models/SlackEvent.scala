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
    val token = (json \ "token").validate[String].asOpt.getOrElse("")

    val `type` = EventType.values.find(x => (json \ "event" \ "type").validate[String].asOpt.contains(x.toString))

    Logger.debug(`type`.toString)
    `type`.map { t =>

      val user =
        if (t == EventType.MESSAGE) {
          (json \ "event" \ "user").validate[String]
        } else {
          (json \ "event" \ "user" \ "id").validate[String]
        }

      user match {
        case JsSuccess(u, _) => JsSuccess(SlackEvent(token, t, u))
        case _ => JsError("Unknown event type")
      }

    }.getOrElse(JsError("Unknown event type"))


  }
}

